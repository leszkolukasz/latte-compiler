use crate::backend::asm::{Operand, Register};
use crate::parser::{Program, TopDef, Type};
use crate::{frontend, Ext};
use anyhow::Result;
use maplit::hashset;
use std::collections::{HashMap, HashSet, LinkedList, VecDeque};
use crate::frontend::Stats;

pub mod asm;
mod block;
mod class;
mod expression;
mod function;
mod optimizations;
mod statement;
mod top_level;
mod utils;

type Var = String;
pub type ASM = LinkedList<asm::Instruction>;

pub const STACK_COUNTER_START: i64 = 1; // 0 is RBP

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Loc {
    Reg(Register),
    Stack(i64), // relative to RBP, later multiplied by -8
}

impl Loc {
    pub fn to_operand(&self) -> Operand {
        match self {
            Loc::Reg(reg) => Operand::Reg(reg.clone()),
            Loc::Stack(offset) => Operand::Mem(Register::RBP, -8 * offset, None, None),
        }
    }

    pub fn to_register(&self) -> Register {
        match self {
            Loc::Reg(reg) => reg.clone(),
            _ => panic!("Cannot convert to Register"),
        }
    }
}

// To be used with `save` and `load` functions.
#[derive(Debug, Clone)]
pub enum Checkpoint {
    Saved(Var),        // Value was in register and was saved.
    NotSaved(Operand, Option<Var>), // Value was in memory or immediate, no need to save (Rule 4).
}

impl Checkpoint {
    pub fn get_var(&self) -> Option<Var> {
        match self {
            Checkpoint::Saved(var) => Some(var.clone()),
            Checkpoint::NotSaved(_, var) => var.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarInfo {
    loc: Loc,
}

#[derive(Debug)]
pub struct ClassInfo {
    pub inherits: Option<Box<String>>,
    pub members: HashMap<String, Type>,
    pub member_offset: HashMap<String, i64>, // Offsets may not be multiple of 8 bytes if bools are present.
    pub vtable: Vec<(String, String)>,       // (class_name, function_name)
    pub size: i64,                           // in bytes
}

#[derive(Debug)]
pub struct Context<'a> {
    class_info: HashMap<String, ClassInfo>,
    var_info: HashMap<Var, Vec<VarInfo>>,
    loc_to_var: HashMap<Loc, Vec<Var>>,
    used_registers: VecDeque<Register>, // Used for knowing which caller-save registers where used. Oldest registers are at the end.
    free_registers: VecDeque<Register>,
    free_stack_locs: Vec<i64>, // Same type of values as in Loc::Stack.
    stack_loc_counter: i64,    // How much space on stack is used for local variables.
    var_counter: i64,          // For generating tmp variable names.
    label_counter: i64,        // For generating labels.
    string_labels: HashMap<String, String>, // For storing string literals.
    new_vars: Vec<HashSet<String>>, // List of vars declared in each block. Popped on block exit.
    class_name: Option<String>,
    routine_name: Option<String>,    // For generating epilogue label.
    used_functions: HashSet<String>, // For including external functions.
    stats: &'a Stats,
}

impl<'a>  Context<'a> {
    pub fn new(program: &Program<Ext>, stats: &'a Stats) -> Self {
        Context {
            class_info: Self::build_class_info(program),
            var_info: HashMap::new(),
            loc_to_var: HashMap::new(),
            used_registers: VecDeque::new(),
            free_registers: Register::usable_registers(),
            free_stack_locs: Vec::new(),
            stack_loc_counter: STACK_COUNTER_START,
            var_counter: 0,
            label_counter: 0,
            string_labels: HashMap::new(),
            new_vars: Vec::new(),
            class_name: None,
            routine_name: None,
            used_functions: HashSet::new(),
            stats,
        }
    }

    fn build_class_info(program: &Program<Ext>) -> HashMap<String, ClassInfo> {
        let mut class_info: HashMap<String, ClassInfo> = HashMap::new();

        for decl in program.iter() {
            let (def, pos, t) = &**decl;

            match def {
                TopDef::Class(c) => {
                    let info = frontend::class::build_class_type(&(c, pos.clone(), t.clone()));
                    if let Type::Class {
                        name,
                        info: Some(info),
                    } = info
                    {
                        class_info.insert(
                            *name.clone(),
                            ClassInfo {
                                inherits: info.inherits,
                                members: info
                                    .members
                                    .iter()
                                    .map(|(k, v)| (k.clone(), v.typ.clone()))
                                    .collect(),
                                member_offset: HashMap::new(),
                                vtable: vec![],
                                size: 8, // size of pointer to vtable
                            },
                        );
                    } else {
                        unreachable!()
                    }
                }
                _ => {}
            }
        }

        let mut visited: HashSet<String> = HashSet::new();

        let keys: Vec<String> = class_info.keys().map(|k| k.clone()).collect();
        for class_name in keys {
            Self::_build_class_info(class_name.clone(), &mut class_info, &mut visited, program);
        }

        class_info
    }

    fn _build_class_info(
        class_name: String,
        class_info: &mut HashMap<String, ClassInfo>,
        visited: &mut HashSet<String>,
        program: &Program<Ext>,
    ) -> (Vec<(String, String)>, i64) {
        let info = class_info.get_mut(&class_name).unwrap();

        if visited.contains(&class_name) {
            return (info.vtable.clone(), info.size);
        }

        visited.insert(class_name.clone());

        if let Some(parent) = &info.inherits {
            let (vtable, size) =
                Self::_build_class_info(*parent.clone(), class_info, visited, program);
            let info = class_info.get_mut(&class_name).unwrap();

            info.vtable = vtable;
            info.size = size;
        }

        let info = class_info.get_mut(&class_name).unwrap();

        for (name, typ) in info.members.iter() {
            match typ {
                Type::Fun(..) => {
                    let mut found = false;
                    for (c, n) in info.vtable.iter_mut() {
                        if n == name {
                            *c = class_name.clone();
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        info.vtable.push((class_name.clone(), name.clone()));
                    }
                }
                _ => {
                    let size = typ.get_size();
                    info.member_offset.insert(name.clone(), info.size);
                    info.size += size as i64;
                }
            }
        }

        (info.vtable.clone(), info.size)
    }

    pub fn new_label(&mut self) -> String {
        self.label_counter += 1;
        format!(".L{}", self.label_counter)
    }

    pub fn new_stack_loc(&mut self) -> Loc {
        if let Some(loc) = self.free_stack_locs.pop() {
            Loc::Stack(loc)
        } else {
            let loc = self.stack_loc_counter;
            self.stack_loc_counter += 1;
            Loc::Stack(loc)
        }
    }

    pub fn new_tmp(&mut self) -> Var {
        self.var_counter += 1;
        format!("%t{}", self.var_counter) // user-defined variables can't start with '%'
    }

    pub fn add_var(&mut self, var: &Var, loc: &Loc, update_new_vars: bool) {
        let vars = self.var_info.entry(var.clone()).or_insert_with(Vec::new);

        vars.push(VarInfo { loc: loc.clone() });

        let locs = self.loc_to_var.entry(loc.clone()).or_insert_with(Vec::new);
        locs.push(var.clone());

        match loc {
            Loc::Reg(reg) => {
                self.free_registers.retain(|r| r != reg);
                self.used_registers.retain(|r| r != reg);
                self.used_registers.push_front(reg.clone());
            }
            Loc::Stack(loc) => {
                self.free_stack_locs.retain(|l| l != loc);
            }
        }

        if update_new_vars {
            self.new_vars.last_mut().unwrap().insert(var.clone());
        }
    }

    #[allow(dead_code)]
    pub fn try_remove_var(&mut self, var: &Var) -> bool {
        if let Some(vars) = self.var_info.get(var) {
            if let Some(_) = vars.last() {
                self.remove_var(var);
                return true;
            }
        }

        false
    }

    pub fn remove_var(&mut self, var: &Var) {
        let vars = self.var_info.get_mut(var).unwrap();
        let info = vars.pop().unwrap();

        let locs = self.loc_to_var.get_mut(&info.loc).unwrap();
        // assert_eq!(locs.last().unwrap(), var);
        // locs.pop();

        locs.retain(|v| v != var);

        match info.loc {
            Loc::Reg(reg) => {
                self.free_registers.push_front(reg);
            }
            Loc::Stack(loc) => {
                self.free_stack_locs.push(loc);
            }
        }
    }

    pub fn move_var(&mut self, var: &Var, new_loc: &Loc) {
        self.remove_var(var);
        self.add_var(var, new_loc, false);
    }

    pub fn get_var_info(&self, var: &Var) -> Option<&VarInfo> {
        self.var_info.get(var).map_or(None, |v| v.last())
    }

    pub fn get_var_by_loc(&self, loc: &Loc) -> Option<&Var> {
        self.loc_to_var.get(loc).map_or(None, |v| v.last())
    }

    // Tries to move variable to other register, or stack if no other register is available.
    fn move_out_of_register(&mut self, var: &Var, disallowed: &HashSet<&Register>) -> ASM {
        let old_info = self.get_var_info(var).unwrap();
        let old_reg = old_info.loc.to_register();

        let mut free_reg: Option<Register> = None;

        for r in self.free_registers.iter() {
            if !disallowed.contains(r) {
                free_reg = Some(r.clone())
            }
        }

        if let Some(free_reg) = free_reg {
            self.move_var(var, &Loc::Reg(free_reg.clone()));

            LinkedList::from([asm::MOV(Operand::Reg(free_reg), Operand::Reg(old_reg))])
        } else {
            let loc = self.new_stack_loc();
            self.move_var(var, &loc);

            LinkedList::from([asm::MOV(loc.to_operand(), Operand::Reg(old_reg))])
        }
    }

    pub fn free_register_with_disallowed(
        &mut self,
        reg: &Register,
        disallowed: &HashSet<&Register>,
    ) -> ASM {
        let mut asm = ASM::new();

        if let Some(var) = self.get_var_by_loc(&Loc::Reg(reg.clone())) {
            asm.append(&mut self.move_out_of_register(&var.clone(), disallowed));
        }

        asm
    }

    pub fn free_register(&mut self, reg: &Register) -> ASM {
        self.free_register_with_disallowed(reg, &hashset! { reg })
    }

    #[allow(dead_code)]
    pub fn free_registers(&mut self, regs: &HashSet<Register>) -> ASM {
        let disallowed: HashSet<&Register> = regs.iter().collect();
        let mut asm = ASM::new();

        for reg in regs {
            asm.append(&mut self.free_register_with_disallowed(reg, &disallowed));
        }

        asm
    }

    pub fn free_any_register(&mut self) -> (Register, ASM) {
        if let Some(reg) = self.free_registers.back() {
            (reg.clone(), self.free_register(&reg.clone()))
        } else {
            // Oldest register is at the end.
            let reg = self.used_registers.back().unwrap_or(&Register::RAX).clone();
            (reg.clone(), self.free_register(&reg))
        }
    }

    pub fn free_any_register_with_disallowed(
        &mut self,
        disallowed: &[&Register],
    ) -> (Register, ASM) {
        let disallowed: HashSet<&Register> = disallowed.iter().cloned().collect();

        if let Some(reg) = self.free_registers.iter().find(|r| !disallowed.contains(r)) {
            (
                reg.clone(),
                self.free_register_with_disallowed(&reg.clone(), &disallowed),
            )
        } else {
            let reg = if let Some(r) = self
                .used_registers
                .iter()
                .rev()
                .find(|r| !disallowed.contains(r))
            {
                r.clone()
            } else {
                Register::usable_registers()
                    .iter()
                    .find(|r| !disallowed.contains(r))
                    .unwrap()
                    .clone()
            };
            (
                reg.clone(),
                self.free_register_with_disallowed(&reg, &disallowed),
            )
        }
    }

    // (is_cycle, path)
    fn dfs(
        graph: &HashMap<Operand, Operand>,
        start: &Operand,
        visited: &mut Vec<Operand>,
    ) -> (bool, Vec<Operand>) {
        let mut path: Vec<Operand> = vec![];

        visited.push(start.clone());
        path.push(start.clone());
        let mut next = graph.get(&start);

        while let Some(n) = next {
            if visited.contains(n) {
                return (true, path);
            }

            path.push(n.clone().clone());
            visited.push(n.clone().clone());
            next = graph.get(n).clone();
        }

        (false, path)
    }

    // Ensures that given operands are in given registers.
    // Tries to perform minimal amount of moves.
    #[allow(suspicious_double_ref_op)]
    pub fn move_to(&mut self, constraints: &[(&Operand, &Register)]) -> ASM {
        let mut cycles: Vec<Vec<Operand>> = vec![];
        let mut paths_that_end_with_free_reg: Vec<Vec<Operand>> = vec![];
        let mut paths_that_end_with_non_free_reg: Vec<Vec<Operand>> = vec![];

        let mut visited: Vec<Operand> = vec![];
        let mut start_points: Vec<Operand> = vec![];
        let mut graph: HashMap<Operand, Operand> = HashMap::new();

        for (from, to) in constraints {
            graph.insert(from.clone().clone(), Operand::Reg(to.clone().clone()));
        }

        // Start points are operands that have in-degree 0.
        for (from, _to) in constraints {
            if !graph.values().any(|v| v == *from) {
                start_points.push(from.clone().clone());
            }
        }

        // Remaining points are on cycles.
        for (from, _to) in constraints {
            if !start_points.contains(from) {
                start_points.push(from.clone().clone());
            }
        }

        for start in start_points {
            if visited.contains(&start.clone()) {
                continue;
            }

            let (is_cycle, path) = Context::dfs(&graph, &start.clone(), &mut visited);
            if is_cycle {
                cycles.push(path);
            } else {
                if let Some(last) = path.last() {
                    if self.free_registers.contains(&last.to_register()) {
                        paths_that_end_with_free_reg.push(path);
                    } else {
                        paths_that_end_with_non_free_reg.push(path);
                    }
                }
            }
        }

        let mut asm = ASM::new();

        for cycle in cycles {
            if cycle.len() == 1 {
                continue;
            }

            // Elements in cycle can only be registers
            for i in 0..cycle.len() - 1 {
                assert!(cycle[cycle.len() - 1 - i].is_reg());
                assert!(cycle[cycle.len() - 2 - i].is_reg());
                asm.push_back(asm::XCHG(
                    cycle[cycle.len() - 1 - i].clone(),
                    cycle[cycle.len() - 2 - i].clone(),
                ));
            }
        }

        for path in paths_that_end_with_free_reg {
            for i in 0..path.len() - 1 {
                asm.push_back(asm::MOV(
                    path[path.len() - 1 - i].clone(),
                    path[path.len() - 2 - i].clone(),
                ));
            }
        }

        // Paths that end on non-free registers should be checked last
        // to maybe reuse some registers.

        // All registers where something is moved to are disallowed.
        let mut disallowed: HashSet<&Register> =
            constraints.iter().map(|(_, r)| r.clone()).collect();

        // All registers at the start of path that end with non-free register are disallowed for now
        // They will be added one by one later.
        for path in &paths_that_end_with_non_free_reg {
            if let Some(Operand::Reg(reg)) = path.first() {
                disallowed.insert(reg);
            }
        }

        for path in &paths_that_end_with_non_free_reg {
            let last = self.get_var_by_loc(&path.last().unwrap().to_loc()).unwrap();
            asm.append(&mut self.move_out_of_register(&last.clone(), &disallowed));
            if let Some(Operand::Reg(reg)) = path.first() {
                disallowed.insert(reg);
            }

            for i in 0..path.len() - 1 {
                asm.push_back(asm::MOV(
                    path[path.len() - 1 - i].clone(),
                    path[path.len() - 2 - i].clone(),
                ));
            }
        }

        asm
    }

    pub fn move_all_to_memory(&mut self) -> ASM {
        let mut asm = ASM::new();

        let mut to_move: Vec<(Var, VarInfo)> = vec![];

        for (var, info) in &self.var_info {
            if let Some(info) = info.last() {
                if let Loc::Reg(_) = &info.loc {
                    to_move.push((var.clone(), info.clone()));
                }
            }
        }

        for (var, info) in to_move {
            let loc = self.new_stack_loc();

            asm.push_back(asm::MOV(loc.to_operand(), info.loc.to_operand()));
            self.move_var(&var, &loc);
        }

        asm
    }

    pub fn get_field_offset(&self, class_name: &String, field_name: &String) -> Option<i64> {
        let info = self.class_info.get(class_name).unwrap();

        if let Some(offset) = info.member_offset.get(field_name) {
            Some(*offset)
        } else {
            if let Some(parent) = &info.inherits {
                self.get_field_offset(parent, field_name)
            } else {
                None
            }
        }
    }

    pub fn get_field_type(&self, class_name: &String, field_name: &String) -> Option<Type> {
        let info = self.class_info.get(class_name).unwrap();

        if let Some(typ) = info.members.get(field_name) {
            Some(typ.clone())
        } else {
            if let Some(parent) = &info.inherits {
                self.get_field_type(parent, field_name)
            } else {
                None
            }
        }
    }

    pub fn get_vtable_pos(&self, class_name: &String, method_name: &String) -> Option<i64> {
        let info = self.class_info.get(class_name).unwrap();

        for (i, (_, m)) in info.vtable.iter().enumerate() {
            if method_name == m {
                return Some(i as i64);
            }
        }

        if let Some(parent) = &info.inherits {
            self.get_vtable_pos(parent, method_name)
        } else {
            None
        }
    }

    pub fn move_to_reg(&mut self, op: &Operand) -> (Register, ASM) {
        assert!(!op.is_reg());

        let (reg, mut asm) = self.free_any_register();
        asm.push_back(asm::MOV(Operand::Reg(reg.clone()), op.clone()));
        (reg, asm)
    }

    // Makes sure given operand is named so that it is not overwritten by other operations.
    // If it needs to be overwritten, e.g. it is in RAX, it will be moved somewhere else.
    pub fn save(&mut self, op: &Operand) -> Checkpoint {
        match op {
            Operand::Reg(_) => {
                if let Some(var) = self.get_var_by_loc(&op.to_loc()) {
                    Checkpoint::NotSaved(op.clone(), Some(var.clone()))
                } else {
                    let tmp = self.new_tmp();
                    self.add_var(&tmp, &op.to_loc(), false);

                    Checkpoint::Saved(tmp)
                }
            },
            Operand::Mem(..) => {
                let var = self.get_var_by_loc(&op.to_loc()).unwrap();
                Checkpoint::NotSaved(op.clone(), Some(var.clone()))
            },
            Operand::Imm(_) => Checkpoint::NotSaved(op.clone(), None)
        }
    }

    pub fn load(&mut self, checkpoint: Checkpoint) -> Operand {
        match checkpoint {
            Checkpoint::Saved(var) => {
                let loc = self.get_var_info(&var).unwrap().loc.to_operand();
                self.remove_var(&var);
                loc
            }
            Checkpoint::NotSaved(op, _) => op,
        }
    }

    // Main difference between this and `save` is that this function will also save Imm values.
    pub fn full_save(
        &mut self,
        op: &Operand,
    ) -> (Checkpoint, ASM) {
        self.full_save_with_disallowed(op, &[])
    }

    pub fn full_save_with_disallowed(
        &mut self,
        op: &Operand,
        disallowed: &[&Register],
    ) -> (Checkpoint, ASM) {
        match op {
            Operand::Reg(_) | Operand::Mem(..) => (self.save(op), ASM::new()),
            Operand::Imm(_) => {
                let (reg, mut asm) = self.free_any_register_with_disallowed(disallowed);
                asm.push_back(asm::MOV(Operand::Reg(reg.clone()), op.clone()));
                (self.save(&Operand::Reg(reg)), asm)
            }
        }
    }

    pub fn ensure_in_register(&mut self, op: &Operand) -> (Operand, ASM) {
        if let Operand::Reg(_) = op {
            (op.clone(), ASM::new())
        } else {
            let (reg, new_asm) = self.move_to_reg(&op);
            (Operand::Reg(reg), new_asm)
        }
    }

    pub fn ensure_in_register_with_disallowed(
        &mut self,
        op: &Operand,
        disallowed: &[&Register],
    ) -> (Operand, ASM) {
        let mut new_disallowed: HashSet<&Register> = disallowed.iter().cloned().collect();

        if let Operand::Reg(r) = op {
            if !disallowed.contains(&r) {
                return (op.clone(), ASM::new());
            }

            new_disallowed.insert(r);
        }

        let (reg, mut new_asm) = self.free_any_register_with_disallowed(
            new_disallowed.into_iter().collect::<Vec<_>>().as_slice(),
        );
        let mut asm = ASM::new();
        asm.append(&mut new_asm);
        asm.push_back(asm::MOV(Operand::Reg(reg.clone()), op.clone()));
        (Operand::Reg(reg), asm)
    }

    pub fn ensure_in_memory(&mut self, op: &Operand) -> (Operand, ASM) {
        if let Operand::Mem(..) = op {
            (op.clone(), ASM::new())
        } else {
            let loc = self.new_stack_loc();
            (
                loc.to_operand(),
                LinkedList::from([asm::MOV(loc.to_operand(), op.clone())]),
            )
        }
    }


    pub fn is_vtable_needed(&self, class_name: &String) -> bool {
        let info = self.class_info.get(class_name).unwrap();

        !info.vtable.is_empty()
    }

    pub fn get_string_label(&mut self, s: &String) -> String {
        if let Some(label) = self.string_labels.get(s) {
            label.clone()
        } else {
            let label = format!(".LS{}", self.string_labels.len() + 1);
            self.string_labels.insert(s.clone(), label.clone());
            label
        }
    }
}

pub fn compile(program: &Program<Ext>, stats: &Stats) -> Result<ASM> {
    let mut context = Context::new(program, stats);
    let asm = top_level::compile(&mut context, &program)?;
    Ok(optimizations::run(asm))
}

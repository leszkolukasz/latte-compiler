use crate::backend::asm::{Operand, Register};
use crate::backend::{asm, block, Context, Loc, ASM, STACK_COUNTER_START};
use crate::common::Enriched;
use crate::parser::Routine;
use crate::Ext;
use anyhow::Result;
use std::cmp::max;

pub const ARG_REGISTERS: [Register; 6] = [
    Register::RDI,
    Register::RSI,
    Register::RDX,
    Register::RCX,
    Register::R8,
    Register::R9,
];

pub fn compile(context: &mut Context, fun: &Enriched<&Routine<Ext>, Ext>) -> Result<ASM> {
    on_enter(context);

    let arg_cnt = add_function_args(context, fun);
    let mut body = block::compile(context, &fun.0.body)?;

    let mut prologue =
        compile_prologue(context, max(arg_cnt as i64 - ARG_REGISTERS.len() as i64, 0));
    let mut epilogue = compile_epilogue(context);

    let mut asm = ASM::new();

    asm.push_back(asm::CUSTOM("".to_string())); // for newline
    asm.push_back(asm::LABEL(context.routine_name.clone().unwrap()));
    asm.append(&mut prologue.0);
    asm.append(&mut body);
    asm.append(&mut epilogue);

    let asm = post_process(context, asm, prologue.1);

    context.routine_name = None;

    on_exit(context);

    Ok(asm)
}

fn add_function_args(context: &mut Context, fun: &Enriched<&Routine<Ext>, Ext>) -> usize {
    if let Some(class_name) = &context.class_name {
        context.routine_name = Some(format!("{}${}", class_name, *fun.0.name.clone()));
        context.add_var(
            &"self".to_string(),
            &Loc::Reg(ARG_REGISTERS[0].clone()),
            true,
        );
    } else {
        if *fun.0.name == "main" {
            context.routine_name = Some("main".to_string());
        } else {
            context.routine_name = Some(format!("_{}", *fun.0.name.clone()));
        }
    }

    let args = &*fun.0.params;
    let mut cnt = if context.class_name.is_some() { 1 } else { 0 };

    for arg in args {
        let (_, name) = &arg.0;

        if cnt < ARG_REGISTERS.len() {
            context.add_var(name, &Loc::Reg(ARG_REGISTERS[cnt].clone()), true);
        } else {
            // + 1 because return address it at 0x8(%rbp)
            let loc = Loc::Stack(-((cnt - (ARG_REGISTERS.len() - 1) + 1) as i64));
            context.add_var(name, &loc, true);
        }

        cnt += 1;
    }

    cnt
}

// Makes sure stack is aligned to 16 bytes
// and fixes any issues caused by callee_save registers.
fn post_process(_context: &mut Context, asm: ASM, shift: usize) -> ASM {
    let mut new_asm = ASM::new();
    let mut alignment = 1; // 16 bytes
    let mut pending_pop = false;

    for instr in asm.into_iter() {
        match instr {
            asm::PUSH(_) | asm::POP(_) => {
                alignment = (alignment + 1) % 2;
                new_asm.push_back(instr);
            }
            asm::ADD(Operand::Reg(Register::RSP), Operand::Imm(imm))
            | asm::SUB(Operand::Reg(Register::RSP), Operand::Imm(imm)) => {
                if imm % 16 != 0 {
                    alignment = (alignment + 1) % 2;
                }
                new_asm.push_back(instr);
            }
            asm::ALIGN_PLACEHOLDER(v) => {
                // current alignment + number of arguments of function
                if (alignment + v) % 2 != 0 {
                    new_asm.push_back(asm::SUB(Operand::Reg(Register::RSP), Operand::Imm(8)));
                    pending_pop = true;
                }
            }
            asm::CALL(_) => {
                new_asm.push_back(instr);
                if pending_pop {
                    new_asm.push_back(asm::ADD(Operand::Reg(Register::RSP), Operand::Imm(8)));
                    pending_pop = false;
                }
            }
            asm::LABEL(ref l) => {
                new_asm.push_back(instr.clone());

                // If this is new function, reset alignment.
                if !l.starts_with(".") {
                    alignment = 1;
                }
            }
            mut i => {
                let mut operands = i.get_operands();
                for op in operands.iter_mut() {
                    if let Operand::Mem(Register::RBP, offset, ..) = op {
                        if offset < &mut 0 {
                            // local variables
                            *offset -= (shift as i64) * 8;
                        } else {
                            // function arguments
                        }
                    }
                }

                match i {
                    asm::CALL_DEREF(_) => {
                        if pending_pop {
                            new_asm
                                .push_back(asm::ADD(Operand::Reg(Register::RSP), Operand::Imm(8)));
                            pending_pop = false;
                        }
                    }
                    _ => {}
                }

                new_asm.push_back(i);
            }
        }
    }

    assert_eq!(pending_pop, false);

    new_asm
}

// Returns: asm, shift in stack_loc_counter caused by callee-save registers.
#[allow(suspicious_double_ref_op)]
fn compile_prologue(context: &mut Context, args_on_stack: i64) -> (ASM, usize) {
    let mut asm = ASM::new();

    let callee_save = context
        .used_registers
        .iter()
        .filter(|r| r.is_callee_save())
        .collect::<Vec<_>>();
    let stack_space = context.stack_loc_counter - STACK_COUNTER_START + (callee_save.len() as i64);

    // If we need some local variables or function args on the stack.
    if stack_space + args_on_stack != 0 {
        asm.push_back(asm::PUSH(Operand::Reg(Register::RBP)));
        asm.push_back(asm::MOV(
            Operand::Reg(Register::RBP),
            Operand::Reg(Register::RSP),
        ));

        if !callee_save.is_empty() {
            for reg in &callee_save {
                asm.push_back(asm::PUSH(Operand::Reg(reg.clone().clone())));
            }
        }

        asm.push_back(asm::SUB(
            Operand::Reg(Register::RSP),
            Operand::Imm(8 * stack_space),
        ));
    }

    (asm, callee_save.len())
}

// Assumes result is in RAX.
#[allow(suspicious_double_ref_op)]
fn compile_epilogue(context: &mut Context) -> ASM {
    let mut asm = ASM::new();

    asm.push_back(asm::LABEL(format!(
        ".{}_epilogue",
        context.routine_name.as_ref().unwrap()
    )));

    let callee_save = context
        .used_registers
        .iter()
        .filter(|r| r.is_callee_save())
        .collect::<Vec<_>>();
    let stack_space = context.stack_loc_counter - STACK_COUNTER_START + (callee_save.len() as i64);

    if stack_space != 0 {
        for reg in callee_save.iter().rev() {
            asm.push_back(asm::POP(Operand::Reg(reg.clone().clone())));
        }

        asm.push_back(asm::LEAVE);
    }

    asm.push_back(asm::RET);
    asm
}

// Just some checks to make sure everything works as expected.
fn on_enter(context: &mut Context) {
    for val in context.var_info.values() {
        assert!(val.is_empty());
    }

    for val in context.loc_to_var.values() {
        assert!(val.is_empty());
    }

    context.var_info.clear();
    context.loc_to_var.clear();

    assert!(context.used_registers.is_empty());
    assert_eq!(
        context.free_registers.iter().collect::<Vec<_>>().sort(),
        Register::usable_registers()
            .iter()
            .collect::<Vec<_>>()
            .sort()
    );
    assert!(context.free_stack_locs.is_empty());
    assert_eq!(context.stack_loc_counter, STACK_COUNTER_START);
    assert_eq!(context.var_counter, 0);
    assert!(context.new_vars.is_empty());
    assert!(context.routine_name.is_none());

    block::on_enter(context);
}

fn on_exit(context: &mut Context) {
    block::on_exit(context);

    context.used_registers.clear();
    context.free_stack_locs.clear();
    context.stack_loc_counter = STACK_COUNTER_START;
    context.var_counter = 0;
    context.routine_name = None;

    // These should be already cleaned up if block scoping is implemented correctly
    // context.var_info
    // context.loc_to_var
    // context.free_registers
    // context.new_vars
}

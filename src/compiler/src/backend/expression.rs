use crate::backend::asm::{Operand, Register};
use crate::backend::function::ARG_REGISTERS;
use crate::backend::top_level::EXTERNAL_FUNCTIONS;
use crate::backend::utils::to_dummy_enriched;
use crate::backend::{asm, statement, utils, Checkpoint, Context, Loc, ASM};
use crate::common::{Enriched, LineCol, PtrList};
use crate::parser::{BinaryOp, Expr, IntValue, Stmt, Type, UnaryOp, Value};
use crate::{Ext, NodeData};
use anyhow::Result;
use maplit::hashset;
use std::cmp::max;
use std::mem::swap;
use std::ops::Deref;

// Assumptions:
// 1) If result operand is register, modifying it will not destroy the state of the program.
// 2) If result is in memory, it can only be memory location on stack with respect to RBP.
// 3) If result is in memory, it cannot be modified in place.
// 4) If result is in memory, it cannot move to any other location or be changed before result is consumed.
// 5) If result is in memory, it has to be named.

#[derive(Debug)]
pub struct LvalInfo {
    op: Option<Operand>, // Operand with address of lvalue of class to which member belongs or None if array was indexed.
                         // Used for adding "self" to method call.
}

// (code, operand with result, lvalue info)
type CompiledExpr = (ASM, Operand, Option<LvalInfo>);

// If lval is true, then returned value is lvalue. This only matters for class members/array indexing.
// Third attribute in CompiledExpr is then set to appropriate data.
// If lval is false, then returned value is rvalue. Third attribute is None.
pub fn compile(
    context: &mut Context,
    expr: &Enriched<Expr<Ext>, Ext>,
    lval: bool,
) -> Result<CompiledExpr> {
    match &expr.0 {
        Expr::Ident(name) => compile_ident(context, name, lval),
        Expr::Literal(lit) => compile_literal(context, lit),
        Expr::Unary(op, expr) => compile_unary(context, op, expr),
        Expr::Binary(op, lhs, rhs) => compile_binary(context, op, lhs, rhs, None),
        Expr::Apply(ident, args) => compile_apply(context, ident, args),
        Expr::Index(lhs, idx) => compile_index(context, lhs, idx, lval),
        Expr::NewArray(t, size) => compile_new_array(context, t, size),
        Expr::NewObject(t) => compile_new_object(context, t),
        Expr::Member(lhs, member) => compile_member(context, lhs, member, lval),
        Expr::Cast(_, expr) => compile(context, expr, lval), // don't need to do anything
    }
}

fn compile_ident(context: &mut Context, ident: &str, lval: bool) -> Result<CompiledExpr> {
    let info = context.get_var_info(&ident.to_string());

    if let Some(info) = info {
        // Local variable
        match &info.loc {
            Loc::Reg(reg) => {
                // Rule (1), modifying register will not destroy the state of the program
                // so value needs to be moved to memory.

                let reg = reg.clone();
                let new_loc = context.new_stack_loc();
                let mut asm = ASM::new();
                asm.push_back(asm::MOV(new_loc.to_operand(), Operand::Reg(reg.clone())));

                context.remove_var(&ident.to_string());
                context.add_var(&ident.to_string(), &new_loc, false);

                Ok((asm, Operand::Reg(reg.clone()), None))
            }
            // This should be the only place that returns Operand::Mem
            Loc::Stack(offset) => Ok((
                ASM::new(),
                Operand::Mem(Register::RBP, -8 * offset, None, None),
                None,
            )),
        }
    } else {
        // Class field
        let class_name = context.class_name.clone().unwrap();
        let lhs: Enriched<Expr<Ext>, Ext> = (
            Expr::Ident(Box::new("self".to_string())),
            LineCol { line: 0, col: 0 },
            NodeData {
                expr_typ: Some(Type::Class {
                    name: Box::new(class_name.clone()),
                    info: None,
                }),
            },
        );

        compile_member(context, &lhs, ident, lval)
    }
}

fn compile_literal(context: &mut Context, val: &Value) -> Result<CompiledExpr> {
    match val {
        Value::Num(v) => {
            let mut asm = ASM::new();

            if v.value() > i32::MAX as i64 || v.value() < i32::MIN as i64 {
                let mut reg = context.free_any_register();
                asm.append(&mut reg.1);
                asm.push_back(asm::MOVABS(
                    Operand::Reg(reg.0.clone()),
                    Operand::Imm(v.value()),
                ));
                Ok((asm, Operand::Reg(reg.0), None))
            } else {
                Ok((asm, Operand::Imm(v.value()), None))
            }
        }
        Value::Bool(b) => Ok((ASM::new(), Operand::Imm(*b as i64), None)),
        Value::Null => Ok((ASM::new(), Operand::Imm(0), None)),
        Value::String(s) => {
            let mut asm = ASM::new();

            // Create String class
            let class_loc = context.new_stack_loc();
            let class_var = context.new_tmp();
            context.add_var(&class_var, &class_loc, false);

            let class_type = Type::Class {
                name: Box::new("String".to_string()),
                info: None,
            };

            let create_string_class: Enriched<Stmt<Ext>, Ext> = utils::to_dummy_enriched(
                Stmt::Assign(
                    Box::new(utils::to_dummy_enriched(
                        Expr::Ident(Box::new(class_var.clone())),
                        Some(class_type.clone()),
                    )),
                    Box::new(utils::to_dummy_enriched(
                        Expr::NewObject(Box::new(Type::Class {
                            name: Box::new("String".to_string()),
                            info: None,
                        })),
                        Some(class_type.clone()),
                    )),
                ),
                None,
            );

            let mut create_string_class_res = statement::compile(context, &create_string_class)?;
            asm.append(&mut create_string_class_res);

            // Create data array
            let create_int8_array = utils::to_dummy_enriched(
                Stmt::Assign(
                    Box::new(utils::to_dummy_enriched(
                        Expr::Member(
                            Box::new(utils::to_dummy_enriched(
                                Expr::Ident(Box::new(class_var.clone())),
                                Some(class_type.clone()),
                            )),
                            Box::new("data".to_string()),
                        ),
                        Some(Type::Array(Box::new(Type::Bool))),
                    )),
                    Box::new(utils::to_dummy_enriched(
                        Expr::NewArray(
                            Box::new(Type::Bool),
                            Box::new(utils::to_dummy_enriched(
                                Expr::Literal(Box::new(Value::Num(IntValue::Parsed(
                                    s.len() as i64
                                )))),
                                Some(Type::Int),
                            )),
                        ),
                        Some(Type::Array(Box::new(Type::Bool))),
                    )),
                ),
                None,
            );

            let mut create_int8_array_res = statement::compile(context, &create_int8_array)?;
            asm.append(&mut create_int8_array_res);

            // Get data array address
            let data_loc = context.new_stack_loc();
            let data_var = context.new_tmp();
            context.add_var(&data_var, &data_loc, false);

            let mut data_var_res = statement::compile(
                context,
                &utils::to_dummy_enriched(
                    Stmt::Assign(
                        Box::new(utils::to_dummy_enriched(
                            Expr::Ident(Box::new(data_var.clone())),
                            Some(Type::Array(Box::new(Type::Bool))),
                        )),
                        Box::new(utils::to_dummy_enriched(
                            Expr::Member(
                                Box::new(utils::to_dummy_enriched(
                                    Expr::Ident(Box::new(class_var.clone())),
                                    Some(class_type.clone()),
                                )),
                                Box::new("data".to_string()),
                            ),
                            Some(Type::Array(Box::new(Type::Bool))),
                        )),
                    ),
                    None,
                ),
            )?;

            asm.append(&mut data_var_res);

            // data_var now contains address of data array, but first element is length
            let data_op = context.get_var_info(&data_var).unwrap().loc.to_operand();
            let (data_op, mut new_asm) = context.ensure_in_memory(&data_op);
            asm.append(&mut new_asm);

            asm.push_back(asm::ADD(data_op.clone(), Operand::Imm(8)));

            // Copy string to data array
            let string_label = context.get_string_label(s);

            // Move string label to RSI now, so that it is not unnecessarily moved in next `context.move_to`
            let label_reg = Register::RSI;
            asm.append(&mut context.free_register(&label_reg));

            asm.push_back(asm::ALIGN_PLACEHOLDER(0));

            asm.push_back(asm::CUSTOM(format!(
                "\tleaq\t{}(%rip), {}",
                string_label,
                label_reg.clone()
            )));
            asm.append(&mut context.move_to(&[
                (&data_op, &Register::RDI),
                (&Operand::Reg(label_reg), &Register::RSI),
                (&Operand::Imm(s.len() as i64), &Register::RDX),
            ]));

            asm.push_back(asm::CALL("memcpy".to_string()));
            context.used_functions.insert("memcpy".to_string());

            let class_op = context.get_var_info(&class_var).unwrap().loc.to_operand();
            context.remove_var(&class_var);
            context.remove_var(&data_var);

            // Rule 5, class_var was removed, so if class_op was in memory it is no longer named.
            let (class_op, mut new_asm) = context.ensure_in_register(&class_op);
            asm.append(&mut new_asm);

            Ok((asm, class_op, None))
        }
    }
}

fn compile_unary(
    context: &mut Context,
    op: &UnaryOp,
    expr: &Enriched<Expr<Ext>, Ext>,
) -> Result<CompiledExpr> {
    let expr_res = compile(context, expr, false)?;

    let mut asm = expr_res.0;

    if let Operand::Imm(v) = expr_res.1 {
        unreachable!(
            "Hmm, this should have been evaluated at type checking stage. v: {}",
            v
        );
    } else if let Operand::Reg(_) = expr_res.1 {
        match op {
            UnaryOp::Not => {
                asm.push_back(asm::XOR(expr_res.1.clone(), Operand::Imm(1)));
            }
            UnaryOp::Neg => {
                asm.push_back(asm::NEG(expr_res.1.clone()));
            }
        }
        Ok((asm, expr_res.1, None))
    } else {
        // Result is in memory, but rule (3) says that it cannot be modified in place.
        let mut reg = context.free_any_register();
        asm.append(&mut reg.1);
        asm.push_back(asm::MOV(Operand::Reg(reg.0.clone()), expr_res.1.clone()));

        match op {
            UnaryOp::Not => {
                asm.push_back(asm::XOR(Operand::Reg(reg.0.clone()), Operand::Imm(1)));
            }
            UnaryOp::Neg => {
                asm.push_back(asm::NEG(Operand::Reg(reg.0.clone())));
            }
        }
        Ok((asm, Operand::Reg(reg.0), None))
    }
}

fn compile_new_object(context: &mut Context, t: &Type) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let vtable_name: Option<String>;
    let size;

    if let Type::Class { name, .. } = t {
        size = context.class_info.get(name.deref()).unwrap().size;
        vtable_name = if context.is_vtable_needed(name) {
            Some(format!("{}_vtable", name))
        } else {
            None
        };
    } else {
        unreachable!()
        // size = t.get_size() as i64;
    }

    asm.append(&mut context.move_to(&[
        (&Operand::Imm(1), &Register::RDI),
        (&Operand::Imm(size), &Register::RSI),
    ]));
    asm.append(&mut context.move_all_to_memory());

    asm.push_back(asm::ALIGN_PLACEHOLDER(0));
    asm.push_back(asm::CALL("calloc".to_string()));

    context.used_functions.insert("calloc".to_string());

    if let Some(vtable_name) = vtable_name {
        // Move vtable ptr
        let mut reg = context.free_any_register_with_disallowed(&[&Register::RAX]);
        asm.append(&mut reg.1);

        asm.push_back(asm::CUSTOM(format!(
            "\tleaq\t{}(%rip), {}",
            vtable_name,
            reg.0.clone()
        )));
        asm.push_back(asm::MOV(
            Operand::Mem(Register::RAX, 0, None, None),
            Operand::Reg(reg.0),
        ));
    }

    Ok((asm, Operand::Reg(Register::RAX), None))
}

fn compile_new_array(
    context: &mut Context,
    t: &Type,
    size: &Enriched<Expr<Ext>, Ext>,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let mut size_res = compile(context, size, false)?;
    asm.append(&mut size_res.0);

    let elem_size = t.get_size() as i64;

    let size_op = if let Operand::Imm(v) = size_res.1 {
        // Make space for array size (8 bytes)
        if elem_size == 1 {
            // If bool, then add 8 more 1-byte elements
            Operand::Imm(v + 8)
        } else {
            assert_eq!(elem_size, 8);
            Operand::Imm(v + 1)
        }
    } else {
        // Moved to memory, because we still need it after calloc
        // TODO: could have used some callee-save register
        let (size_op, mut new_asm) = context.ensure_in_memory(&size_res.1);
        asm.append(&mut new_asm);

        // Make space for array size (8 bytes)
        if elem_size == 1 {
            // If bool, then add 8 more 1-byte elements
            asm.push_back(asm::ADD(size_op.clone(), Operand::Imm(8)));
        } else {
            assert_eq!(elem_size, 8);
            asm.push_back(asm::INC(size_op.clone()));
        }

        size_op
    };

    asm.append(&mut context.move_to(&[
        (&size_op, &Register::RDI),
        (&Operand::Imm(elem_size), &Register::RSI),
    ]));
    asm.append(&mut context.move_all_to_memory());

    asm.push_back(asm::ALIGN_PLACEHOLDER(0));
    asm.push_back(asm::CALL("calloc".to_string()));

    context.used_functions.insert("calloc".to_string());

    let mut reg = context.free_any_register_with_disallowed(&[&Register::RAX]);
    asm.append(&mut reg.1);

    let size_op = if let Operand::Imm(v) = size_op {
        if elem_size == 1 {
            Operand::Imm(v - 8)
        } else {
            Operand::Imm(v - 1)
        }
    } else {
        assert!(size_op.is_mem());

        if elem_size == 1 {
            asm.push_back(asm::SUB(size_op.clone(), Operand::Imm(8)));
        } else {
            asm.push_back(asm::DEC(size_op.clone()));
        }

        size_op
    };

    asm.push_back(asm::MOV(Operand::Reg(reg.0.clone()), size_op));

    // Move size to the beginning of the array
    asm.push_back(asm::MOV(
        Operand::Mem(Register::RAX, 0, None, None),
        Operand::Reg(reg.0.clone()),
    ));

    Ok((asm, Operand::Reg(Register::RAX), None))
}

fn compile_index(
    context: &mut Context,
    lhs: &Enriched<Expr<Ext>, Ext>,
    idx: &Enriched<Expr<Ext>, Ext>,
    lval: bool,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let mut idx_res = compile(context, idx, false)?;
    asm.append(&mut idx_res.0);

    let idx_checkpoint = context.save(&idx_res.1);

    let mut lhs_res = compile(context, lhs, false)?;
    asm.append(&mut lhs_res.0);

    let (lhs_op, mut new_asm) = context.ensure_in_register(&lhs_res.1);
    asm.append(&mut new_asm);

    // TODO: can be optimized if idx is imm
    let idx_loaded = context.load(idx_checkpoint.clone());
    let (idx_op, mut new_asm) =
        context.ensure_in_register_with_disallowed(&idx_loaded, &[&lhs_op.to_register()]);
    asm.append(&mut new_asm);

    if let Type::Array(t) = lhs.2.expr_typ.clone().unwrap() {
        let (elem_size, _elem_typ) = (t.get_size() as i64, t.clone());

        if lval {
            asm.push_back(asm::CUSTOM(format!(
                "\tleaq\t0x8({},{},{}), {}",
                lhs_op, idx_op, elem_size, lhs_op
            )));
            Ok((asm, lhs_op.clone(), Some(LvalInfo { op: None })))
        } else {
            if elem_size == 1 {
                asm.push_back(asm::CUSTOM(format!(
                    "\tmovzxb\t0x8({},{},{}), {}",
                    lhs_op, idx_op, elem_size, lhs_op
                )));
            } else {
                asm.push_back(asm::CUSTOM(format!(
                    "\tmovq\t0x8({},{},{}), {}",
                    lhs_op, idx_op, elem_size, lhs_op
                )));
            }
            Ok((asm, lhs_op.clone(), None))
        }
    } else if Type::Str == lhs.2.expr_typ.clone().unwrap() {
        // compile str.data[idx]

        let res_loc = context.new_stack_loc();
        let res_var = context.new_tmp();
        context.add_var(&res_var, &res_loc, false);

        let (lhs_checkpoint, mut new_asm) = if let Operand::Reg(ref r) = idx_op {
            context.full_save_with_disallowed(&lhs_op, &[r])
        } else {
            context.full_save(&lhs_op)
        };

        asm.append(&mut new_asm);

        let (idx_checkpoint, mut new_asm) = context.full_save(&idx_op);
        asm.append(&mut new_asm);

        let lhs_var = lhs_checkpoint.get_var().unwrap();
        let idx_var = idx_checkpoint.get_var().unwrap();

        let string_class_type = Type::Class {
            name: Box::new("String".to_string()),
            info: None,
        };

        let index_member = utils::to_dummy_enriched(
            Stmt::Assign(
                Box::new(utils::to_dummy_enriched(
                    Expr::Ident(Box::new(res_var.clone())),
                    Some(string_class_type.clone()),
                )),
                Box::new(utils::to_dummy_enriched(
                    Expr::Index(
                        Box::new(utils::to_dummy_enriched(
                            Expr::Member(
                                Box::new(utils::to_dummy_enriched(
                                    Expr::Ident(Box::new(lhs_var.clone())),
                                    Some(string_class_type),
                                )),
                                Box::new("data".to_string()),
                            ),
                            Some(Type::Array(Box::new(Type::Bool))),
                        )),
                        Box::new(utils::to_dummy_enriched(
                            Expr::Ident(Box::new(idx_var.clone())),
                            Some(Type::Int),
                        )),
                    ),
                    Some(Type::Bool),
                )),
            ),
            None,
        );

        let mut get_member_res = statement::compile(context, &index_member)?;
        asm.append(&mut get_member_res);

        let res_op = context.get_var_info(&res_var).unwrap().loc.to_operand();

        // Rule 5
        let (res_op, mut new_asm) = context.ensure_in_register(&res_op);
        asm.append(&mut new_asm);

        context.remove_var(&res_var);
        context.load(lhs_checkpoint);
        context.load(idx_checkpoint);

        Ok((asm, res_op, None))
    } else {
        unreachable!();
    }
}

fn compile_member(
    context: &mut Context,
    lhs: &Enriched<Expr<Ext>, Ext>,
    member: &str,
    lval: bool,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let mut lhs_res = compile(context, lhs, false)?;
    asm.append(&mut lhs_res.0);

    let lhs_checkpoint = context.save(&lhs_res.1);

    // NOTE: we can't override lhs_res.1 if it is in register
    let (res, mut new_asm) = if let Operand::Reg(r) = &lhs_res.1 {
        context.ensure_in_register_with_disallowed(&lhs_res.1, &[r])
    } else {
        context.ensure_in_register(&lhs_res.1)
    };

    asm.append(&mut new_asm);

    if let Type::Class {
        name: class_name, ..
    } = lhs.2.expr_typ.clone().unwrap()
    {
        if let Some(offset) = context.get_field_offset(&class_name, &member.to_string()) {
            let member_type = context
                .get_field_type(&class_name, &member.to_string())
                .unwrap();

            if lval {
                // Load only address of the field
                asm.push_back(asm::LEA(
                    res.clone(),
                    Operand::Mem(res.to_register(), offset, None, None),
                ));
                return Ok((
                    asm,
                    res,
                    Some(LvalInfo {
                        op: Some(context.load(lhs_checkpoint)),
                    }),
                ));
            } else {
                let mut reg = context.free_any_register();
                asm.append(&mut reg.1);

                if member_type.get_size() == 1 {
                    asm.push_back(asm::MOVZXB(
                        Operand::Reg(reg.0.clone()),
                        Operand::Mem(res.to_register(), offset, None, None),
                    ));
                } else {
                    asm.push_back(asm::MOV(
                        Operand::Reg(reg.0.clone()),
                        Operand::Mem(res.to_register(), offset, None, None),
                    ));
                }

                // Checkpoints need to be loaded to free memory.
                context.load(lhs_checkpoint);
                return Ok((asm, Operand::Reg(reg.0), None));
            }
        } else if let Some(vtable_pos) = context.get_vtable_pos(&class_name, &member.to_string()) {
            assert!(lval);

            // Dereference vtable pointer
            asm.push_back(asm::MOV(
                res.clone(),
                Operand::Mem(res.to_register(), 0, None, None),
            ));

            // Load address of the method
            asm.push_back(asm::MOV(
                res.clone(),
                Operand::Mem(res.to_register(), vtable_pos * 8, None, None),
            ));

            return Ok((
                asm,
                res,
                Some(LvalInfo {
                    op: Some(context.load(lhs_checkpoint)),
                }),
            ));
        } else {
            unreachable!()
        }
    } else if let Type::Array(_) = lhs.2.expr_typ.clone().unwrap() {
        if member == "length" {
            if lval {
                unreachable!("Cannot take address of array length");
            } else {
                context.load(lhs_checkpoint);

                asm.push_back(asm::MOV(
                    res.clone(),
                    Operand::Mem(res.to_register(), 0, None, None),
                ));
                return Ok((asm, res.clone(), None));
            }
        } else {
            unreachable!()
        }
    } else if lhs.2.expr_typ.clone().unwrap() == Type::Str {
        if member == "length" {
            if lval {
                unreachable!("Cannot take address of string length");
            } else {
                context.load(lhs_checkpoint);

                asm.push_back(asm::MOV(
                    res.clone(),
                    Operand::Mem(res.to_register(), 8, None, None),
                )); // dereference string class
                asm.push_back(asm::MOV(
                    res.clone(),
                    Operand::Mem(res.to_register(), 0, None, None),
                )); // dereference data array, + 8 to skip vtable

                return Ok((asm, res.clone(), None));
            }
        }
        unreachable!()
    } else {
        unreachable!()
    }
}

macro_rules! gen_jmps {
    ($asm_res: ident, $lhs_op: ident, $rhs_op: ident, $res_op: ident, $context: ident, $jmp_labels: expr, $instr: ident) => {
        $asm_res.push_back(asm::CMP($lhs_op, $rhs_op));

        if let Some((label_false, label_true)) = $jmp_labels {
            $asm_res.push_back(asm::$instr(label_true));
            $asm_res.push_back(asm::JMP(label_false));
        } else {
            let label_true = $context.new_label();
            let label_end = $context.new_label();
            $asm_res.push_back(asm::$instr(label_true.clone()));
            $asm_res.push_back(asm::MOV($res_op.clone(), Operand::Imm(0)));
            $asm_res.push_back(asm::JMP(label_end.clone()));
            $asm_res.push_back(asm::LABEL(label_true));
            $asm_res.push_back(asm::MOV($res_op.clone(), Operand::Imm(1)));
            $asm_res.push_back(asm::LABEL(label_end));
        };
    };
}

// jmp_labels: (label_false, label_true)
pub fn compile_binary(
    context: &mut Context,
    op: &BinaryOp,
    lhs: &Enriched<Expr<Ext>, Ext>,
    rhs: &Enriched<Expr<Ext>, Ext>,
    jmp_labels: Option<(String, String)>,
) -> Result<CompiledExpr> {
    if op == &BinaryOp::And || op == &BinaryOp::Or {
        return compile_lazy_binary(context, op, lhs, rhs, jmp_labels);
    }

    let mut op = op.clone();
    let mut asm_res = ASM::new();

    let mut lhs_res = compile(context, lhs, false)?;
    asm_res.append(&mut lhs_res.0);

    // If result is in register, it needs to be saved
    // in case it is overwritten by the right-hand side.
    let lhs_checkpoint = context.save(&lhs_res.1);

    let mut rhs_res = compile(context, rhs, false)?;
    asm_res.append(&mut rhs_res.0);

    if let Operand::Imm(lv) = lhs_res.1 {
        if let Operand::Imm(rv) = rhs_res.1 {
            unreachable!(
                "Hmm, this should have been evaluated at type checking stage. lv: {}, rv: {}",
                lv, rv
            );
        }
    }

    let mut lhs_op = context.load(lhs_checkpoint.clone());
    let mut rhs_op = rhs_res.1.clone();

    let mut lhs_typ = lhs.2.expr_typ.clone().unwrap();
    let mut rhs_typ = rhs.2.expr_typ.clone().unwrap();

    // This causes so many bugs!!!
    if !lhs_op.is_reg()
        && rhs_op.is_reg()
        && op.can_swap()
        && lhs_typ != Type::Str
        && rhs_typ != Type::Str
    {
        swap(&mut lhs_op, &mut rhs_op);
        swap(&mut lhs_typ, &mut rhs_typ);

        op = op.reverse_direction();
    }

    // lhs needs to be in register
    let (lhs_op, mut new_asm) = if let Operand::Reg(r) = &rhs_op {
        context.ensure_in_register_with_disallowed(&lhs_op, &[r])
    } else {
        context.ensure_in_register(&lhs_op)
    };

    asm_res.append(&mut new_asm);

    // By default, result is in lhs register
    let mut res_op = lhs_op.clone();

    match op {
        BinaryOp::Add => {
            if let Type::Int = lhs_typ {
                asm_res.push_back(asm::ADD(lhs_op, rhs_op))
            } else {
                let mut res =
                    compile_string_call_method(context, lhs_op, rhs_op, "add".to_string())?;
                asm_res.append(&mut res.0);
                res_op = res.1;
            }
        }
        BinaryOp::Sub => asm_res.push_back(asm::SUB(lhs_op, rhs_op)),
        BinaryOp::Mul => {
            if let Type::Str = lhs_typ {
                let mut res =
                    compile_string_call_method(context, lhs_op, rhs_op, "times".to_string())?;
                asm_res.append(&mut res.0);
                res_op = res.1;
            } else if let Type::Str = rhs_typ {
                let mut res =
                    compile_string_call_method(context, rhs_op, lhs_op, "times".to_string())?;
                asm_res.append(&mut res.0);
                res_op = res.1;
            } else {
                // Int multiplication
                asm_res.push_back(asm::IMUL(lhs_op, rhs_op))
            }
        }
        BinaryOp::Mod | BinaryOp::Div => {
            let rhs_op = if rhs_op.is_imm() || rhs_op.is_reg() {
                let disallowed = if let Operand::Reg(r) = &lhs_op {
                    vec![&Register::RAX, &Register::RDX, r]
                } else {
                    vec![&Register::RAX, &Register::RDX]
                };

                let (op, mut new_asm) =
                    context.ensure_in_register_with_disallowed(&rhs_op, &disallowed);
                asm_res.append(&mut new_asm);
                op
            } else {
                rhs_op
            };

            asm_res.append(&mut context.move_to(&[(&lhs_op, &Register::RAX)]));

            let mut disallowed = hashset! { &Register::RAX };
            if let Operand::Reg(r) = &rhs_op {
                disallowed.insert(r);
            }

            asm_res.append(&mut context.free_register_with_disallowed(&Register::RDX, &disallowed));

            asm_res.push_back(asm::CQO);
            asm_res.push_back(asm::DIV(rhs_op));

            res_op = if op == BinaryOp::Div {
                Operand::Reg(Register::RAX)
            } else {
                Operand::Reg(Register::RDX)
            };
        }
        BinaryOp::Eq => {
            if let Type::Str = lhs_typ {
                let mut res =
                    compile_string_call_method(context, lhs_op, rhs_op, "eq".to_string())?;
                asm_res.append(&mut res.0);
                res_op = res.1;
            } else {
                gen_jmps!(
                    asm_res,
                    lhs_op,
                    rhs_op,
                    res_op,
                    context,
                    jmp_labels.clone(),
                    JE
                );
            }
        }
        BinaryOp::Neq => {
            if let Type::Str = lhs_typ {
                let mut res =
                    compile_string_call_method(context, lhs_op, rhs_op, "eq".to_string())?;
                asm_res.append(&mut res.0);
                res_op = res.1;

                asm_res.push_back(asm::XOR(res_op.clone(), Operand::Imm(1)));
            } else {
                gen_jmps!(
                    asm_res,
                    lhs_op,
                    rhs_op,
                    res_op,
                    context,
                    jmp_labels.clone(),
                    JNE
                );
            }
        }
        BinaryOp::Lt => {
            gen_jmps!(
                asm_res,
                lhs_op,
                rhs_op,
                res_op,
                context,
                jmp_labels.clone(),
                JL
            );
        }
        BinaryOp::Lte => {
            gen_jmps!(
                asm_res,
                lhs_op,
                rhs_op,
                res_op,
                context,
                jmp_labels.clone(),
                JLE
            );
        }
        BinaryOp::Gt => {
            gen_jmps!(
                asm_res,
                lhs_op,
                rhs_op,
                res_op,
                context,
                jmp_labels.clone(),
                JG
            );
        }
        BinaryOp::Gte => {
            gen_jmps!(
                asm_res,
                lhs_op,
                rhs_op,
                res_op,
                context,
                jmp_labels.clone(),
                JGE
            );
        }
        BinaryOp::And | BinaryOp::Or => unreachable!(),
    }

    Ok((asm_res, res_op, None))
}

fn compile_lazy_binary(
    context: &mut Context,
    op: &BinaryOp,
    lhs: &Enriched<Expr<Ext>, Ext>,
    rhs: &Enriched<Expr<Ext>, Ext>,
    jmp_labels: Option<(String, String)>,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let label_false;
    let label_true;
    let label_mid = context.new_label();
    let label_end = context.new_label();

    // Use provided labels if they exist
    if let Some((label_false_, label_true_)) = &jmp_labels {
        label_false = label_false_.clone();
        label_true = label_true_.clone();
    } else {
        label_false = context.new_label();
        label_true = context.new_label();
    }

    // If labels were provided then this value is ignored,
    // otherwise a new free register will be found later;
    let mut operand: Operand = Operand::Reg(Register::RAX);

    compile_lazy_binary_single_side(
        context,
        op,
        &mut asm,
        lhs,
        label_true.clone(),
        label_false.clone(),
        label_mid.clone(),
        false,
    )?;

    asm.push_back(asm::LABEL(label_mid.clone()));

    compile_lazy_binary_single_side(
        context,
        op,
        &mut asm,
        rhs,
        label_true.clone(),
        label_false.clone(),
        label_mid.clone(),
        true,
    )?;

    if !jmp_labels.is_some() {
        let mut reg = context.free_any_register();
        asm.append(&mut reg.1);
        operand = Operand::Reg(reg.0.clone());

        asm.push_back(asm::LABEL(label_true));
        asm.push_back(asm::MOV(operand.clone(), Operand::Imm(1)));
        asm.push_back(asm::JMP(label_end.clone()));
        asm.push_back(asm::LABEL(label_false));
        asm.push_back(asm::MOV(operand.clone(), Operand::Imm(0)));
        asm.push_back(asm::LABEL(label_end));
    }

    Ok((asm, operand, None))
}

fn compile_string_call_method(
    context: &mut Context,
    lhs_op: Operand,
    rhs_op: Operand,
    method: String,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let (lhs_checkpoint, mut new_asm) = if let Operand::Reg(r) = &rhs_op {
        context.full_save_with_disallowed(&lhs_op, &[r])
    } else {
        context.full_save(&lhs_op)
    };

    asm.append(&mut new_asm);

    let (rhs_checkpoint, mut new_asm) = context.full_save(&rhs_op);
    asm.append(&mut new_asm);

    let lhs_var = lhs_checkpoint.get_var().unwrap();
    let rhs_var = rhs_checkpoint.get_var().unwrap();

    let string_type = Type::Class {
        name: Box::new("String".to_string()),
        info: None,
    };

    // Call lhs.`method`(rhs)
    let mut args = PtrList::new();
    args.push(Box::new(utils::to_dummy_enriched(
        Expr::Ident(Box::new(rhs_var.clone())),
        Some(string_type.clone()),
    )));

    let call: Enriched<Expr<Ext>, Ext> = utils::to_dummy_enriched(
        Expr::Apply(
            Box::new(utils::to_dummy_enriched(
                Expr::Member(
                    Box::new(utils::to_dummy_enriched(
                        Expr::Ident(Box::new(lhs_var.clone())),
                        Some(string_type.clone()),
                    )),
                    Box::new(method),
                ),
                None,
            )),
            Box::new(args),
        ),
        None,
    );

    let mut res = compile(context, &call, false)?;
    context.load(lhs_checkpoint);
    context.load(rhs_checkpoint);

    asm.append(&mut res.0);

    Ok((asm, res.1, res.2))
}

fn compile_lazy_binary_single_side(
    context: &mut Context,
    op: &BinaryOp,
    asm: &mut ASM,
    expr: &Enriched<Expr<Ext>, Ext>,
    label_true: String,
    label_false: String,
    label_mid: String,
    last: bool,
) -> Result<()> {
    if let Expr::Binary(inner_op, lhs, rhs) = &expr.0 {
        let where_to_jump_if_false = match op {
            BinaryOp::And => label_false.clone(),
            BinaryOp::Or if last => label_false.clone(),
            BinaryOp::Or if !last => label_mid.clone(),
            _ => unreachable!(),
        };

        let where_to_jump_if_true = match op {
            BinaryOp::And if last => label_true.clone(),
            BinaryOp::And if !last => label_mid.clone(),
            BinaryOp::Or => label_true.clone(),
            _ => unreachable!(),
        };

        let mut exp_res = compile_binary(
            context,
            inner_op,
            lhs,
            rhs,
            Some((where_to_jump_if_false, where_to_jump_if_true)),
        )?;
        asm.append(&mut exp_res.0);

        Ok(())
    } else {
        let mut expr_res = compile(context, expr, false)?;
        asm.append(&mut expr_res.0);

        let (operand, mut new_asm) = context.ensure_in_register(&expr_res.1);
        asm.append(&mut new_asm);

        match op {
            BinaryOp::And => {
                asm.push_back(asm::TEST(operand.clone(), operand.clone()));
                asm.push_back(asm::JE(label_false.clone()));

                if last {
                    asm.push_back(asm::JMP(label_true.clone()));
                }
            }
            BinaryOp::Or => {
                asm.push_back(asm::TEST(operand.clone(), operand.clone()));
                asm.push_back(asm::JNE(label_true.clone()));

                if last {
                    asm.push_back(asm::JMP(label_false.clone()));
                }
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}

#[derive(Debug)]
enum CallType {
    Method(LvalInfo, Checkpoint), // lval info of class, checkpoint for method address variable
    Function(String),
}

pub fn compile_apply(
    context: &mut Context,
    ident: &Enriched<Expr<Ext>, Ext>,
    args: &PtrList<Enriched<Expr<Ext>, Ext>>,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let call_type: CallType;

    // Method call without self
    let is_implicit_method = if context.class_name.is_none() {
        false
    } else if let Expr::Ident(name) = &ident.0 {
        if context
            .get_vtable_pos(&context.class_name.clone().unwrap(), name)
            .is_some()
        {
            true
        } else {
            false
        }
    } else {
        false
    };

    match &ident.0 {
        Expr::Ident(name) if !is_implicit_method => {
            context.used_functions.insert(*name.clone());

            match name.as_str() {
                "printString" => {
                    let arg = args.get(0).unwrap();
                    return compile_apply_printString(context, arg);
                }
                _ => {}
            }

            call_type = CallType::Function(*name.clone());
        }
        _ => {
            let method_expr = if is_implicit_method {
                let Expr::Ident(name) = &ident.0 else { unreachable!() };
                to_dummy_enriched(
                    Expr::Member(
                        Box::new(to_dummy_enriched(
                            Expr::Ident(Box::new("self".to_string())),
                            Some(Type::Class {
                                name: Box::new(context.class_name.clone().unwrap()),
                                info: None,
                            }),
                        )),
                        name.clone(),
                    ),
                    None,
                )
            } else {
                ident.clone()
            };

            let mut ident_res = compile(context, &method_expr, true)?;
            asm.append(&mut ident_res.0);

            // Assert that this is class member
            assert!(ident_res.2.is_some());

            call_type = CallType::Method(ident_res.2.unwrap(), context.save(&ident_res.1));
        }
    };

    let mut arg_res: Vec<CompiledExpr> = Vec::new();
    let mut arg_checkpoints: Vec<Checkpoint> = Vec::new();

    // If method, add self as first argument
    if let CallType::Method(lval_info, _) = &call_type {
        arg_checkpoints.push(context.save(&lval_info.op.clone().unwrap()));
        arg_res.push((ASM::new(), lval_info.op.clone().unwrap(), None));
    }

    // Compile code for arguments
    for arg in args {
        let mut res = compile(context, arg, false)?;
        asm.append(&mut res.0);

        arg_checkpoints.push(context.save(&res.1));
        arg_res.push(res);
    }

    asm.push_back(asm::ALIGN_PLACEHOLDER(max(
        arg_res.len() as i64 - ARG_REGISTERS.len() as i64,
        0,
    )));

    // Move arguments that do not fit in registers to stack
    if arg_res.len() > ARG_REGISTERS.len() {
        for (idx, _) in arg_res.iter().enumerate().rev() {
            if idx < ARG_REGISTERS.len() {
                break;
            }

            let checkpoint = &arg_checkpoints[idx];
            let operand = context.load(checkpoint.clone());

            asm.push_back(asm::PUSH(operand));
        }
    }

    // Move rest to registers
    let mut arg_operands: Vec<Operand> = vec![];
    for (idx, _) in arg_res.iter().enumerate() {
        if idx >= ARG_REGISTERS.len() {
            break;
        }

        let checkpoint = &arg_checkpoints[idx];
        arg_operands.push(context.load(checkpoint.clone()));
    }

    asm.append(
        &mut context.move_to(
            arg_operands
                .iter()
                .zip(ARG_REGISTERS.iter())
                .collect::<Vec<_>>()
                .as_slice(),
        ),
    );

    // This needs to be loaded before `move_all_to_memory` if it is in register it will be
    // unnecessarily moved to memory.
    let fun_addr = match &call_type {
        CallType::Method(_, fun_addr_var) => Some(context.load(fun_addr_var.clone())),
        CallType::Function(_) => None,
    };

    // TODO: callee-save registers can be skipped
    // Everything is moved to memory, so that no variables are overwritten.
    asm.append(&mut context.move_all_to_memory());

    // Stack alignment is happening later after whole function body is compiled.

    if let CallType::Function(fun_name) = call_type {
        if EXTERNAL_FUNCTIONS.contains(&fun_name.as_ref()) {
            asm.push_back(asm::CALL(fun_name));
        } else {
            asm.push_back(asm::CALL(format!("_{}", fun_name)));
        }
    } else if let CallType::Method(_, _) = call_type {
        asm.push_back(asm::CALL_DEREF(fun_addr.unwrap()));
    }

    // Cleanup stack
    if arg_res.len() > ARG_REGISTERS.len() {
        let stack_space = 8 * (arg_res.len() - ARG_REGISTERS.len());
        asm.push_back(asm::ADD(
            Operand::Reg(Register::RSP),
            Operand::Imm(stack_space as i64),
        ));
    }

    Ok((asm, Operand::Reg(Register::RAX), None))
}

#[allow(non_snake_case)]
pub fn compile_apply_printString(
    context: &mut Context,
    expr: &Enriched<Expr<Ext>, Ext>,
) -> Result<CompiledExpr> {
    let mut asm = ASM::new();

    let mut expr_res = compile(context, expr, false)?;
    asm.append(&mut expr_res.0);

    // Load address of string and size of string
    let (data_op, mut new_asm) =
        context.ensure_in_register_with_disallowed(&expr_res.1, &[&Register::RDI, &Register::RSI]);
    asm.append(&mut new_asm);

    // dereference
    asm.push_back(asm::MOV(
        data_op.clone(),
        Operand::Mem(data_op.clone().to_register(), 8, None, None),
    )); // + 8 to skip vtable

    asm.push_back(asm::MOV(
        Operand::Reg(Register::RSI),
        Operand::Mem(data_op.clone().to_register(), 0, None, None),
    ));
    asm.push_back(asm::LEA(
        Operand::Reg(Register::RDI),
        Operand::Mem(data_op.clone().to_register(), 8, None, None),
    ));

    asm.push_back(asm::ALIGN_PLACEHOLDER(0));
    asm.push_back(asm::CALL("printString".to_string()));

    context.used_functions.insert("printString".to_string());

    Ok((asm, Operand::Reg(Register::RAX), None))
}

pub fn maybe_dereference(res: &CompiledExpr) -> Operand {
    let mut op = res.1.clone();
    // Returned value is heap location stored in register, it needs to be dereferenced.
    if res.2.is_some() {
        if let Operand::Reg(r) = op {
            op = Operand::Mem(r, 0, None, None);
        } else {
            unreachable!("Returned lvalue is not in register");
        }
    }
    op
}

use crate::backend::asm::{Operand, Register};
use crate::backend::utils::to_dummy_enriched;
use crate::backend::{asm, block, expression, Context, ASM};
use crate::common::{Enriched, PtrList};
use crate::parser::{BinaryOp, Block, DeclItem, Expr, IntValue, Stmt, Type, Value};
use crate::{Ext, NodeData};
use anyhow::Result;

pub fn compile(context: &mut Context, stmt: &Enriched<Stmt<Ext>, Ext>) -> Result<ASM> {
    match &stmt.0 {
        Stmt::Empty => Ok(ASM::new()),
        Stmt::Block(b) => block::compile(context, b),
        Stmt::Decl(typ, items) => compile_decl(context, typ, items),
        Stmt::Expr(expr) => Ok(expression::compile(context, expr, false)?.0),
        Stmt::Assign(lhs, rhs) => compile_assign(context, lhs, rhs),
        Stmt::Incr(expr) => compile_incr_decr(context, expr, true),
        Stmt::Decr(expr) => compile_incr_decr(context, expr, false),
        Stmt::ReturnVoid => compile_return_void(context),
        Stmt::Return(expr) => compile_return(context, expr),
        Stmt::Cond(expr, branch) => compile_cond(context, expr, branch),
        Stmt::CondElse(expr, branch_true, branch_false) => {
            compile_cond_else(context, expr, branch_true, Some(branch_false))
        }
        Stmt::While(cond, body) => compile_while(context, cond, body),
        Stmt::For(t, ident, arr, body) => compile_for(context, t, ident, arr, body),
    }
}

fn compile_decl(
    context: &mut Context,
    typ: &Type,
    items: &PtrList<Enriched<DeclItem<Ext>, Ext>>,
) -> Result<ASM> {
    let mut asm = ASM::new();

    for item in items {
        let (loc, mut expr_res) = match &item.0 {
            DeclItem::NoInit(name) => {
                let loc = context.new_stack_loc();
                context.add_var(name, &loc, true);

                let default_val = match typ {
                    Type::Int => Value::Num(IntValue::Parsed(0)),
                    Type::Bool => Value::Bool(false),
                    Type::Class { .. } => Value::Null,
                    Type::Array(_) => Value::Null,
                    Type::Str => Value::String(Box::new("".to_string())),
                    _ => unimplemented!(),
                };

                let expr: Enriched<Expr<Ext>, Ext> = (
                    Expr::Literal(Box::new(default_val)),
                    item.1.clone(),
                    NodeData {
                        expr_typ: Some(typ.clone()),
                    },
                );
                (loc, expression::compile(context, &expr, false)?)
            }
            DeclItem::Init(name, expr) => {
                let res = expression::compile(context, expr, false)?;

                // Ident needs to be added after compiling expression, to handle code like: int i = i + 1;
                let loc = context.new_stack_loc();
                context.add_var(name, &loc, true);

                (loc, res)
            }
        };

        asm.append(&mut expr_res.0);

        let mut expr_op = expr_res.1.clone();
        if expr_res.1.is_mem() {
            let (op, mut new_asm) = context.ensure_in_register(&expr_op);
            asm.append(&mut new_asm);
            expr_op = op;
        }

        asm.push_back(asm::MOV(loc.to_operand(), expr_op));
    }

    Ok(asm)
}

fn compile_assign(
    context: &mut Context,
    lhs: &Enriched<Expr<Ext>, Ext>,
    rhs: &Enriched<Expr<Ext>, Ext>,
) -> Result<ASM> {
    let mut asm = ASM::new();

    let mut rhs_res = expression::compile(context, rhs, false)?;
    asm.append(&mut rhs_res.0);

    let rhs_checkpoint = context.save(&rhs_res.1);

    let mut lhs_res = expression::compile(context, lhs, true)?;
    asm.append(&mut lhs_res.0);

    let lhs_op = expression::maybe_dereference(&lhs_res);

    let mut rhs_op = context.load(rhs_checkpoint);

    let is_bool_heap_assignment = if let Operand::Mem(r, ..) = &lhs_op {
        if r != &Register::RBP {
            lhs.2.expr_typ.clone().unwrap().get_size() == 1
        } else {
            false
        }
    } else {
        false
    };

    // If both operands are memory locations, rhs needs to be loaded into register.
    // If bool assignment, movzx needs operands to be in registers.
    if lhs_op.is_mem() && rhs_op.is_mem() || is_bool_heap_assignment {
        let (op, mut new_asm) = if is_bool_heap_assignment {
            // RSI, and RDI don't have 1 byte subregisters
            context.ensure_in_register_with_disallowed(
                &rhs_op,
                &[&lhs_op.get_inner_register(), &Register::RDI, &Register::RSI],
            )
        } else {
            context.ensure_in_register_with_disallowed(&rhs_op, &[&lhs_op.get_inner_register()])
        };

        asm.append(&mut new_asm);
        rhs_op = op;
    }

    if is_bool_heap_assignment {
        if let Operand::Reg(r) = rhs_op {
            asm.push_back(asm::CUSTOM(format!("\tmovb\t{}, {}", r.to_1byte(), lhs_op)));
        } else {
            unreachable!();
        }
    } else {
        asm.push_back(asm::MOV(lhs_op.clone(), rhs_op.clone()));
    }

    Ok(asm)
}

fn compile_incr_decr(
    context: &mut Context,
    expr: &Enriched<Expr<Ext>, Ext>,
    incr: bool,
) -> Result<ASM> {
    let stmt = Stmt::Assign(
        Box::new(expr.clone()),
        Box::new((
            Expr::Binary(
                if incr { BinaryOp::Add } else { BinaryOp::Sub },
                Box::new(expr.clone()),
                Box::new((
                    Expr::Literal(Box::new(Value::Num(IntValue::Parsed(1)))),
                    expr.1.clone(),
                    NodeData {
                        expr_typ: Some(Type::Int),
                    },
                )),
            ),
            expr.1.clone(),
            NodeData { expr_typ: None },
        )),
    );

    compile(
        context,
        &(stmt, expr.1.clone(), NodeData { expr_typ: None }),
    )
}

fn compile_cond(
    context: &mut Context,
    expr: &Enriched<Expr<Ext>, Ext>,
    branch: &Enriched<Stmt<Ext>, Ext>,
) -> Result<ASM> {
    compile_cond_else(context, expr, branch, None)
}

fn compile_cond_else(
    context: &mut Context,
    expr: &Enriched<Expr<Ext>, Ext>,
    branch_true: &Enriched<Stmt<Ext>, Ext>,
    branch_false: Option<&Enriched<Stmt<Ext>, Ext>>,
) -> Result<ASM> {
    let mut asm = ASM::new();

    let label_true = context.new_label();
    let label_false = context.new_label();
    let label_end = context.new_label();

    let where_jump_if_false = if branch_false.is_some() {
        label_false.clone()
    } else {
        label_end.clone()
    };

    // If expression is binary, we can optimize by jumping directly to the correct branch.
    if let Expr::Binary(op, lhs, rhs) = &expr.0 {
        let mut expr_res = expression::compile_binary(
            context,
            op,
            lhs,
            rhs,
            Some((where_jump_if_false, label_true.clone())),
        )?;

        asm.append(&mut expr_res.0);

        // Because we don't know which branch will be executed, we need to move all variables to memory
        // so that all variables are in the same location in both branches and after them.
        asm.append(&mut context.move_all_to_memory());

        asm.push_back(asm::LABEL(label_true));

        block::on_enter(context);
        asm.append(&mut compile(context, branch_true)?);
        block::on_exit(context);

        if let Some(branch_false) = branch_false {
            asm.push_back(asm::JMP(label_end.clone()));

            asm.push_back(asm::LABEL(label_false));

            block::on_enter(context);
            asm.append(&mut compile(context, branch_false)?);
            block::on_exit(context);
        }

        asm.push_back(asm::LABEL(label_end));

        Ok(asm)
    } else {
        let mut expr_res = expression::compile(context, expr, false)?;
        asm.append(&mut expr_res.0);

        if let Operand::Imm(0) = expr_res.1 {
            return if let Some(branch_false) = branch_false {
                block::on_enter(context);
                let res = compile(context, branch_false)?;
                block::on_exit(context);
                Ok(res)
            } else {
                Ok(asm)
            };
        } else if let Operand::Imm(1) = expr_res.1 {
            block::on_enter(context);
            let res = compile(context, branch_true)?;
            block::on_exit(context);

            return Ok(res);
        }

        asm.append(&mut context.move_all_to_memory());

        let (operand, mut new_asm) = context.ensure_in_register(&expr_res.1);
        asm.append(&mut new_asm);

        asm.push_back(asm::TEST(operand.clone(), operand.clone()));
        asm.push_back(asm::JE(where_jump_if_false));

        block::on_enter(context);
        asm.append(&mut compile(context, branch_true)?);
        block::on_exit(context);

        if let Some(branch_false) = branch_false {
            asm.push_back(asm::JMP(label_end.clone()));
            asm.push_back(asm::LABEL(label_false));

            block::on_enter(context);
            asm.append(&mut compile(context, branch_false)?);
            block::on_exit(context);
        }

        asm.push_back(asm::LABEL(label_end));

        Ok(asm)
    }
}

fn compile_while(
    context: &mut Context,
    cond: &Enriched<Expr<Ext>, Ext>,
    body: &Enriched<Stmt<Ext>, Ext>,
) -> Result<ASM> {
    let mut asm = ASM::new();

    let label_body = context.new_label();
    let label_cond = context.new_label();

    asm.append(&mut context.move_all_to_memory());
    asm.push_back(asm::JMP(label_cond.clone()));
    asm.push_back(asm::LABEL(label_body.clone()));

    block::on_enter(context);
    asm.append(&mut compile(context, body)?);
    block::on_exit(context);

    asm.append(&mut context.move_all_to_memory());
    asm.push_back(asm::LABEL(label_cond.clone()));

    // If expression is binary, we can optimize by jumping directly to the correct branch.
    if let Expr::Binary(op, lhs, rhs) = &cond.0 {
        let label_end = context.new_label();
        let mut expr_res = expression::compile_binary(
            context,
            op,
            lhs,
            rhs,
            Some((label_end.clone(), label_body.clone())),
        )?;

        asm.append(&mut expr_res.0);
        asm.push_back(asm::LABEL(label_end));
    } else {
        let mut cond_res = expression::compile(context, cond, false)?;
        asm.append(&mut cond_res.0);

        if let Operand::Imm(0) = cond_res.1 {
            return Ok(asm);
        } else if let Operand::Imm(1) = cond_res.1 {
            asm.push_back(asm::JMP(label_body.clone()));
            return Ok(asm);
        }

        let (op, mut new_asm) = context.ensure_in_register(&cond_res.1);
        asm.append(&mut new_asm);

        asm.push_back(asm::TEST(op.clone(), op.clone()));
        asm.push_back(asm::JNE(label_body.clone()));
    }

    Ok(asm)
}

fn compile_for(
    context: &mut Context,
    t: &Type,
    ident: &str,
    arr: &Enriched<Expr<Ext>, Ext>,
    body: &Enriched<Stmt<Ext>, Ext>,
) -> Result<ASM> {
    let mut asm = ASM::new();

    // {
    // int i=0, len = arr.length;
    // while (i < len) {
    //      t ident = arr[i];
    //      { body }
    //      i++;
    // }
    // }

    let idx_loc = context.new_stack_loc();
    let len_loc = context.new_stack_loc();

    let idx_var = context.new_tmp();
    let len_var = context.new_tmp();

    context.add_var(&idx_var, &idx_loc, false);
    context.add_var(&len_var, &len_loc, false);

    let mut arr_res = expression::compile(context, arr, false)?;
    asm.append(&mut arr_res.0);

    let (arr_var, clean_arr) = if arr_res.1.clone().is_mem() {
        (
            context
                .get_var_by_loc(&arr_res.1.clone().to_loc())
                .unwrap()
                .clone(),
            false,
        )
    } else {
        let arr_var = context.new_tmp();
        context.add_var(&arr_var, &arr_res.1.clone().to_loc(), false);
        (arr_var, true)
    };

    let mut block_stmts: Block<Ext> = PtrList::new();

    // This could be a string or array
    let arr_var_type = arr.2.expr_typ.clone().unwrap();

    let mut decl_ptr_list: PtrList<Enriched<DeclItem<Ext>, Ext>> = PtrList::new();
    decl_ptr_list.push(Box::new(to_dummy_enriched(
        DeclItem::Init(
            Box::new(idx_var.clone()),
            Box::new(to_dummy_enriched(
                Expr::Literal(Box::new(Value::Num(IntValue::Parsed(0)))),
                Some(Type::Int),
            )),
        ),
        None,
    )));
    decl_ptr_list.push(Box::new(to_dummy_enriched(
        DeclItem::Init(
            Box::new(len_var.clone()),
            Box::new(to_dummy_enriched(
                Expr::Member(
                    Box::new(to_dummy_enriched(
                        Expr::Ident(Box::new(arr_var.clone())),
                        Some(arr_var_type.clone()),
                    )),
                    Box::new("length".to_string()),
                ),
                Some(Type::Int),
            )),
        ),
        None,
    )));
    let decl_stmt: Enriched<Stmt<Ext>, Ext> = to_dummy_enriched(
        Stmt::Decl(Box::new(Type::Int), Box::new(decl_ptr_list)),
        None,
    );
    block_stmts.push(Box::new(decl_stmt));

    let mut while_block: Block<Ext> = PtrList::new();

    let item_decl: Enriched<DeclItem<Ext>, Ext> = to_dummy_enriched(
        DeclItem::Init(
            Box::new(ident.to_string()),
            Box::new(to_dummy_enriched(
                Expr::Index(
                    Box::new(to_dummy_enriched(
                        Expr::Ident(Box::new(arr_var.clone())),
                        Some(arr_var_type.clone()),
                    )),
                    Box::new(to_dummy_enriched(
                        Expr::Ident(Box::new(idx_var.clone())),
                        Some(Type::Int),
                    )),
                ),
                Some(t.clone()),
            )),
        ),
        None,
    );
    let mut item_decl_list: PtrList<Enriched<DeclItem<Ext>, Ext>> = PtrList::new();
    item_decl_list.push(Box::new(item_decl));
    while_block.push(Box::new(to_dummy_enriched(
        Stmt::Decl(Box::new(t.clone()), Box::new(item_decl_list)),
        None,
    )));

    let mut inner_block_list: Block<Ext> = PtrList::new();
    inner_block_list.push(Box::new(body.clone()));
    while_block.push(Box::new(to_dummy_enriched(
        Stmt::Block(Box::new(inner_block_list)),
        None,
    )));

    let incr_stmt: Enriched<Stmt<Ext>, Ext> = to_dummy_enriched(
        Stmt::Incr(Box::new(to_dummy_enriched(
            Expr::Ident(Box::new(idx_var.clone())),
            Some(Type::Int),
        ))),
        None,
    );
    while_block.push(Box::new(incr_stmt));

    let while_stmt: Enriched<Stmt<Ext>, Ext> = to_dummy_enriched(
        Stmt::While(
            Box::new(to_dummy_enriched(
                Expr::Binary(
                    BinaryOp::Lt,
                    Box::new(to_dummy_enriched(
                        Expr::Ident(Box::new(idx_var.clone())),
                        Some(Type::Int),
                    )),
                    Box::new(to_dummy_enriched(
                        Expr::Ident(Box::new(len_var.clone())),
                        Some(Type::Int),
                    )),
                ),
                None,
            )),
            Box::new(to_dummy_enriched(
                Stmt::Block(Box::new(while_block.clone())),
                None,
            )),
        ),
        None,
    );

    block_stmts.push(Box::new(while_stmt));

    let stmt = to_dummy_enriched(Stmt::Block(Box::new(block_stmts)), None);
    let mut stmt_res = compile(context, &stmt)?;
    asm.append(&mut stmt_res);

    context.remove_var(&idx_var);
    context.remove_var(&len_var);

    if clean_arr {
        context.remove_var(&arr_var);
    }

    Ok(asm)
}

fn compile_return(context: &mut Context, expr: &Enriched<Expr<Ext>, Ext>) -> Result<ASM> {
    let expr_res = expression::compile(context, expr, false)?;

    let mut asm = expr_res.0;

    asm.append(&mut context.move_to(&[(&expr_res.1, &Register::RAX)]));

    asm.push_back(asm::JMP(format!(
        ".{}_epilogue",
        context.routine_name.as_ref().unwrap()
    )));
    Ok(asm)
}

fn compile_return_void(context: &mut Context) -> Result<ASM> {
    let mut asm = ASM::new();

    asm.push_back(asm::JMP(format!(
        ".{}_epilogue",
        context.routine_name.as_ref().unwrap()
    )));
    Ok(asm)
}

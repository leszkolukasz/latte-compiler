use crate::common::{Enriched, LineCol, PtrList};
use crate::frontend::error::TypecheckerError;
use crate::frontend::function::ReturnInfo;
use crate::frontend::types::assert_assignable;
use crate::frontend::{block, expression, keywords, types, Context, TypeInfo};
use crate::parser::{DeclItem, Expr, Stmt, Type, Value};
use crate::Ext;
use anyhow::{bail, Result};

pub fn check(context: &mut Context, stmt: &mut Enriched<Stmt<Ext>, Ext>) -> Result<ReturnInfo> {
    match &mut stmt.0 {
        Stmt::Empty => Ok(ReturnInfo::NoReturn(None)),
        Stmt::Block(b) => block::check(context, b),
        Stmt::Decl(typ, items) => check_decl(context, typ, items),
        Stmt::Expr(expr) => check_expr(context, expr),
        Stmt::Assign(lval, rval) => check_assign(context, lval, rval),
        Stmt::Incr(expr) => check_incr_decr(context, expr),
        Stmt::Decr(expr) => check_incr_decr(context, expr),
        Stmt::ReturnVoid => Ok(ReturnInfo::CertainReturn(
            stmt.1.clone(),
            Box::new(Type::Void),
        )),
        Stmt::Return(expr) => check_return(context, expr),
        Stmt::Cond(cond, if_branch) => check_cond(context, cond, if_branch, None),
        Stmt::CondElse(cond, if_branch, else_branch) => {
            check_cond(context, cond, if_branch, Some(else_branch))
        }
        Stmt::While(cond, body) => check_while(context, cond, body),
        Stmt::For(typ, ident, array, body) => check_for(context, typ, ident, array, body, &stmt.1),
    }
}

pub fn check_expr(
    context: &mut Context,
    expr: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ReturnInfo> {
    let expr_info = expression::check(context, expr)?;

    if expr_info.is_error {
        Ok(ReturnInfo::CertainReturn(
            expr.1.clone(),
            Box::new(Type::Any),
        ))
    } else {
        Ok(ReturnInfo::NoReturn(None))
    }
}

pub fn check_return(
    context: &mut Context,
    expr: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ReturnInfo> {
    let expr_info = expression::check(context, expr)?;

    if expr_info.typ == Type::Void {
        bail!(TypecheckerError::InvalidVoidUsage(Some(expr.1.clone())))
    }

    Ok(ReturnInfo::CertainReturn(
        expr.1.clone(),
        Box::new(expr_info.typ),
    ))
}

pub fn check_while(
    context: &mut Context,
    cond: &mut Enriched<Expr<Ext>, Ext>,
    stmt: &mut Enriched<Stmt<Ext>, Ext>,
) -> Result<ReturnInfo> {
    let cond_info = expression::check(context, cond)?;
    types::assert_one_of(&cond_info.typ, &[Type::Bool], cond.1.clone())?;

    // See: `check_for` for explanation
    block::on_enter(context);
    let stmt_info = check(context, stmt)?;
    block::on_exit(context);

    let evaluated = expression::try_evaluate(cond)?;

    if evaluated.is_some() {
        let b = match evaluated.unwrap() {
            Value::Bool(b) => b,
            _ => unreachable!(),
        };

        // Replace expression with computed value
        cond.0 = Expr::Literal(Box::new(Value::Bool(b)));

        if b {
            Ok(stmt_info)
        } else {
            Ok(stmt_info.weaken())
        }
    } else {
        // Body of loop may execute or not so we need to weaken return type.
        Ok(stmt_info.weaken())
    }
}

pub fn check_for(
    context: &mut Context,
    typ: &Type,
    ident: &String,
    array: &mut Enriched<Expr<Ext>, Ext>,
    stmt: &mut Enriched<Stmt<Ext>, Ext>,
    pos: &LineCol,
) -> Result<ReturnInfo> {
    keywords::check(context, ident, &pos)?;
    types::check(context, typ, pos)?;

    let expr_info = expression::check(context, array)?;

    let item_type = expr_info.typ.get_array_item_type();
    if item_type.is_none() {
        bail!(TypecheckerError::NotIndexable(
            Some(array.1.clone()),
            expr_info.typ.clone()
        ));
    }

    let item_type = item_type.unwrap();
    if !&item_type.is_assignable_to(typ, context) {
        bail!(TypecheckerError::InvalidType(
            Some(array.1.clone()),
            item_type,
            vec![typ.clone()]
        ))
    }

    // Trick to simulate block to handle code like `for(...) int x;`
    // Note: this means `for(int i: ...) int i = i;` is legal.
    block::on_enter(context);

    context.add_to_scope(
        &ident,
        &TypeInfo {
            typ: typ.clone(),
            pos: Some(pos.clone()),
            is_global: false,
        },
    );

    // This `on_enter` is necessary. This will basically transform `for(int x : arr) <STMT>` into
    // {
    //    x = arr[i]
    //    {
    //      <STMT>
    //    }
    // }
    block::on_enter(context);
    let return_info = check(context, stmt)?;
    block::on_exit(context);

    block::on_exit(context);

    Ok(return_info)
}

pub fn check_cond(
    context: &mut Context,
    cond: &mut Enriched<Expr<Ext>, Ext>,
    if_branch: &mut Enriched<Stmt<Ext>, Ext>,
    else_branch: Option<&mut Enriched<Stmt<Ext>, Ext>>,
) -> Result<ReturnInfo> {
    let cond_info = expression::check(context, cond)?;
    types::assert_one_of(&cond_info.typ, &[Type::Bool], cond.1.clone())?;

    block::on_enter(context);
    let if_branch_info = check(context, if_branch)?;
    block::on_exit(context);

    block::on_enter(context);
    let else_branch_info = if else_branch.is_some() {
        Some(check(context, else_branch.unwrap())?)
    } else {
        None
    };
    block::on_exit(context);

    if let Some(ref else_branch_info) = else_branch_info {
        // Check if branches have compatible return type
        ReturnInfo::get_common_data(&if_branch_info, else_branch_info, context)?;
    }

    let evaluated = expression::try_evaluate(cond)?;

    if evaluated.is_some() {
        let b = match evaluated.unwrap() {
            Value::Bool(b) => b,
            _ => unreachable!(),
        };

        // Replace expression with computed value
        cond.0 = Expr::Literal(Box::new(Value::Bool(b)));

        if b {
            Ok(if_branch_info)
        } else {
            Ok(else_branch_info.unwrap_or_else(move || {
                // There is no else branch but return statement might have appeared in if_branch,
                // so return type info needs to be propagated.

                let ret_pos = if_branch_info.get_pos().map_or(None, |r| Some(r));
                let ret_type = if_branch_info.get_type();
                let v = if ret_pos.is_some() {
                    Some((ret_pos.unwrap().clone(), Box::new(ret_type.clone())))
                } else {
                    None
                };

                ReturnInfo::NoReturn(v)
            }))
        }
    } else {
        if_branch_info.and(
            &else_branch_info.unwrap_or(ReturnInfo::NoReturn(None)),
            context,
        )
    }
}

pub fn check_decl(
    context: &mut Context,
    typ: &Type,
    items: &mut PtrList<Enriched<DeclItem<Ext>, Ext>>,
) -> Result<ReturnInfo> {
    for item in items {
        let name = item.0.get_name();

        keywords::check(context, &name, &item.1)?;
        types::check(context, typ, &item.1)?;

        if context.is_redeclaration(&name) {
            let first_declaration = context.get_from_scope(&name, &item.1)?.unwrap();
            bail!(TypecheckerError::RepeatedDeclaration {
                prev_pos: first_declaration.pos.clone(),
                pos: Some(item.1.clone()),
                name: name.clone(),
            });
        }

        if let DeclItem::Init(_, val) = &mut item.as_mut().0 {
            let val_info = expression::check(context, val)?;
            assert_assignable(context, typ, &val_info.typ, true, &item.1)?;
        }

        context.add_to_scope(
            &name,
            &TypeInfo {
                typ: typ.clone(),
                pos: Some(item.1.clone()),
                is_global: false,
            },
        );
    }

    Ok(ReturnInfo::NoReturn(None))
}

pub fn check_incr_decr(
    context: &mut Context,
    expr: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ReturnInfo> {
    let expr_info = expression::check(context, expr)?;
    if !expr_info.is_lvalue {
        bail!(TypecheckerError::NotAnLValue(Some(expr.1.clone())))
    }

    types::assert_one_of(&expr_info.typ, &[Type::Int], expr.1.clone())?;

    Ok(ReturnInfo::NoReturn(None))
}

pub fn check_assign(
    context: &mut Context,
    lval: &mut Enriched<Expr<Ext>, Ext>,
    rval: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ReturnInfo> {
    let lval_info = expression::check(context, lval)?;
    let rval_info = expression::check(context, rval)?;

    assert_assignable(
        context,
        &lval_info.typ,
        &rval_info.typ,
        lval_info.is_lvalue,
        &lval.1,
    )?;

    Ok(ReturnInfo::NoReturn(None))
}

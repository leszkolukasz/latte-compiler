use crate::common::{Enriched, LineCol, PtrList};
use crate::frontend::class::find_class_member;
use crate::frontend::error::TypecheckerError;
use crate::frontend::{types, Context};
use crate::parser::Type::{Bool, Int};
use crate::parser::{BinaryOp, Expr, IntValue, Type, UnaryOp, Value};
use crate::{Ext, NodeData};
use anyhow::{bail, Result};
use paste::paste;

pub struct ExprInfo {
    pub typ: Type,
    pub is_lvalue: bool,
    pub is_error: bool, // true if `error` builtin was called
}

pub fn check(context: &mut Context, expr: &mut Enriched<Expr<Ext>, Ext>) -> Result<ExprInfo> {
    let expr_info = match &mut expr.0 {
        Expr::Ident(name) => check_ident(context, &name, &expr.1),
        Expr::Literal(lit) => check_lit(context, lit, &expr.1),
        Expr::Unary(op, exp) => {
            let (replace, info) = check_unary(context, op, exp)?;
            if let Some(r) = replace {
                expr.0 = r;
            }
            Ok(info)
        }
        Expr::Binary(op, lhs, rhs) => check_binary(context, op, lhs, rhs),
        Expr::Apply(ident, args) => check_apply(context, ident, args),
        Expr::Index(lval, rval) => check_index(context, lval, rval),
        Expr::NewArray(typ, size) => check_new_array(context, typ, size, &expr.1),
        Expr::NewObject(typ) => check_new_object(context, &typ, &expr.1),
        Expr::Member(obj, name) => check_member(context, obj, name, &expr.1),
        Expr::Cast(typ, expr) => check_cast(context, typ, expr),
    }?;

    let eval = try_evaluate(expr)?;
    expr.2 = NodeData {
        expr_typ: Some(expr_info.typ.clone()),
    }; // Store type for backend

    if let Some(v) = eval {
        expr.0 = Expr::Literal(Box::new(v.clone()));

        assert_eq!(expr_info.typ, v.get_type());
        return Ok(expr_info);
    }

    Ok(expr_info)
}

fn check_ident(context: &mut Context, name: &str, pos: &LineCol) -> Result<ExprInfo> {
    let type_info = context.get_from_scope(name, pos)?;

    if type_info.is_none() {
        bail!(TypecheckerError::UndeclaredIdent(
            Some(pos.clone()),
            name.into()
        ))
    }

    if let Some(type_info) = type_info {
        if let Type::Class { info, .. } = &type_info.typ {
            if info.is_some() {
                bail!(TypecheckerError::CannotUseInExpression(
                    Some(pos.clone()),
                    name.into()
                ))
            }
        }
    }

    Ok(ExprInfo {
        typ: type_info.unwrap().typ.clone(),
        is_lvalue: true,
        is_error: name == "error",
    })
}

fn check_lit(context: &mut Context, val: &Value, pos: &LineCol) -> Result<ExprInfo> {
    let typ = val.get_type();
    types::check(context, &typ, pos)?;

    if let Value::Num(n) = val {
        if let IntValue::Raw(r) = n {
            let r_clone = r.clone();
            if !r.parse::<i64>().is_ok() {
                bail!(TypecheckerError::IntOutOfRange(Some(pos.clone()), *r_clone))
            }
        }
    } else if let Value::String(_) = val {
        context.stats.is_string_used = true;
    }

    Ok(ExprInfo {
        typ,
        is_lvalue: false,
        is_error: false,
    })
}

fn check_unary(
    context: &mut Context,
    op: &UnaryOp,
    expr: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<(Option<Expr<Ext>>, ExprInfo)> {
    // When parsing int literal we check if they are in range -2^63 to 2^63 - 1,
    // but int literals are only positive. Negative literals are parsed as unary negation of positive literals.
    // So value -2^63 needs to handled separately.
    if let UnaryOp::Neg = op {
        if let Expr::Literal(lit) = &expr.0 {
            if let Value::Num(IntValue::Raw(r)) = lit.as_ref() {
                if r.as_ref() == "9223372036854775808" {
                    return Ok((
                        Some(Expr::Literal(Box::new(Value::Num(IntValue::Parsed(
                            -9223372036854775808,
                        ))))),
                        ExprInfo {
                            typ: Type::Int,
                            is_lvalue: false,
                            is_error: false,
                        },
                    ));
                }
            }
        }
    }

    let expr_info = check(context, expr)?;

    match op {
        UnaryOp::Not => types::assert_one_of(&expr_info.typ, &[Bool], expr.1.clone())?,
        UnaryOp::Neg => types::assert_one_of(&expr_info.typ, &[Int], expr.1.clone())?,
    }

    Ok((None, expr_info))
}

fn check_binary(
    context: &mut Context,
    op: &BinaryOp,
    lhs: &mut Enriched<Expr<Ext>, Ext>,
    rhs: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ExprInfo> {
    let lhs_info = check(context, lhs)?;
    let rhs_info = check(context, rhs)?;

    let allowed_combo = op.get_type_combo();
    let allowed_inputs = allowed_combo
        .iter()
        .map(|(lhs, rhs, _)| (lhs.clone(), rhs.clone()))
        .collect::<Vec<_>>();

    types::assert_one_of_tuples(
        &(lhs_info.typ.clone(), rhs_info.typ.clone()),
        &allowed_inputs,
        lhs.1.clone(),
    )?;

    if let Type::Class { name: lname, .. } = &lhs_info.typ {
        if let Type::Class { name: rname, .. } = &rhs_info.typ {
            if !lhs_info.typ.is_assignable_to(&rhs_info.typ, context)
                && !rhs_info.typ.is_assignable_to(&lhs_info.typ, context)
            {
                bail!(TypecheckerError::IncompatibleClasses(
                    Some(lhs.1.clone()),
                    *lname.clone(),
                    *rname.clone()
                ));
            }
        }
    }

    Ok(ExprInfo {
        typ: get_ret_type_for_binary_op(op, &lhs_info.typ, &rhs_info.typ)?,
        is_lvalue: false,
        is_error: false,
    })
}

fn check_apply(
    context: &mut Context,
    ident: &mut Enriched<Expr<Ext>, Ext>,
    args: &mut PtrList<Enriched<Expr<Ext>, Ext>>,
) -> Result<ExprInfo> {
    let ident_info = check(context, ident)?;
    let args_info = args
        .iter_mut()
        .map(|arg| check(context, arg))
        .collect::<Result<Vec<_>>>()?;

    if let Type::Fun(ret_type, param_types) = &ident_info.typ {
        let extracted_param_types = param_types
            .iter()
            .map(|param| *param.clone())
            .collect::<Vec<_>>();
        let extracted_arg_types = args_info
            .iter()
            .map(|arg| arg.typ.clone())
            .collect::<Vec<_>>();

        let mut error = extracted_param_types.len() != extracted_arg_types.len();

        if !error {
            for i in 0..extracted_param_types.len() {
                let param = extracted_param_types.get(i).unwrap();
                let arg = extracted_arg_types.get(i).unwrap();

                if !arg.is_assignable_to(param, context) {
                    error = true;
                    break;
                }
            }
        }

        if error {
            bail!(TypecheckerError::CallArgsMismatch(
                Some(ident.1.clone()),
                extracted_arg_types,
                extracted_param_types
            ));
        }

        return Ok(ExprInfo {
            typ: *ret_type.clone(),
            is_lvalue: false,
            is_error: ident_info.is_error,
        });
    }

    bail!(TypecheckerError::NotCallable(
        Some(ident.1.clone()),
        ident_info.typ.clone()
    ));
}

fn check_index(
    context: &mut Context,
    lval: &mut Enriched<Expr<Ext>, Ext>,
    rval: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ExprInfo> {
    let rval_info = check(context, rval)?;
    types::assert_one_of(&rval_info.typ, &[Int], rval.1.clone())?;

    let lval_info = check(context, lval)?;
    let array_type = lval_info.typ.get_array_item_type();
    if let Some(t) = array_type {
        Ok(ExprInfo {
            typ: t,
            is_lvalue: lval_info.typ.is_array(), // strings should be immutable
            is_error: false,
        })
    } else {
        bail!(TypecheckerError::NotIndexable(
            Some(lval.1.clone()),
            lval_info.typ.clone()
        ));
    }
}

fn check_member(
    context: &mut Context,
    obj: &mut Enriched<Expr<Ext>, Ext>,
    name: &str,
    pos: &LineCol,
) -> Result<ExprInfo> {
    let obj_info = check(context, obj)?;

    if let Type::Class { name: cls_name, .. } = &obj_info.typ {
        if let Some(member) = find_class_member(context, &cls_name, name, pos)? {
            return Ok(ExprInfo {
                typ: member.typ.clone(),
                is_lvalue: !member.typ.is_fun(),
                is_error: false,
            });
        }
    } else if let Type::Array(..) = obj_info.typ {
        if name == "length" {
            return Ok(ExprInfo {
                typ: Int,
                is_lvalue: false,
                is_error: false,
            });
        }
    } else if let Type::Str = obj_info.typ {
        if name == "length" {
            return Ok(ExprInfo {
                typ: Int,
                is_lvalue: false,
                is_error: false,
            });
        }
    }

    bail!(TypecheckerError::NotAMember(
        Some(pos.clone()),
        obj_info.typ.clone(),
        name.into()
    ))
}

fn check_new_array(
    context: &mut Context,
    typ: &Type,
    size: &mut Enriched<Expr<Ext>, Ext>,
    pos: &LineCol,
) -> Result<ExprInfo> {
    types::check(context, typ, pos)?;

    let size_info = check(context, size)?;
    types::assert_one_of(&size_info.typ, &[Int], size.1.clone())?;

    Ok(ExprInfo {
        typ: Type::Array(Box::new(typ.clone())),
        is_lvalue: false,
        is_error: false,
    })
}

fn check_new_object(context: &mut Context, typ: &Type, pos: &LineCol) -> Result<ExprInfo> {
    types::check(context, typ, pos)?;

    match typ {
        Type::Class { .. } => Ok(ExprInfo {
            typ: typ.clone(),
            is_lvalue: false,
            is_error: false,
        }),
        _ => bail!(TypecheckerError::NotAClass(Some(pos.clone()), typ.clone())),
    }
}

fn check_cast(
    context: &mut Context,
    typ: &Type,
    expr: &mut Enriched<Expr<Ext>, Ext>,
) -> Result<ExprInfo> {
    types::check(context, typ, &expr.1)?;

    let expr_info = check(context, expr)?;

    if !expr_info.typ.is_assignable_to(typ, context) {
        bail!(TypecheckerError::CannotCast(
            Some(expr.1.clone()),
            expr_info.typ.clone(),
            typ.clone()
        ));
    }

    Ok(ExprInfo {
        typ: typ.clone(),
        is_lvalue: expr_info.is_lvalue,
        is_error: expr_info.is_error,
    })
}

fn get_ret_type_for_binary_op(op: &BinaryOp, lhs: &Type, rhs: &Type) -> Result<Type> {
    for combo in op.get_type_combo() {
        if &combo.0 == lhs && &combo.1 == rhs {
            return Ok(combo.2.clone());
        }
    }

    bail!("No appropriate return type for binary operation found. This should not happen.")
}

pub fn try_evaluate(expr: &Enriched<Expr<Ext>, Ext>) -> Result<Option<Value>> {
    // If type is set, then expr was already evaluated
    if expr.2.expr_typ.is_some() {
        if let Expr::Literal(v) = &expr.0 {
            return Ok(Some(*v.clone()));
        }

        return Ok(None);
    }

    match &expr.0 {
        Expr::Literal(lit) => Ok(Some(*lit.clone())),
        Expr::Unary(op, expr) => try_evaluate_unary_op(op.clone(), expr),
        Expr::Binary(op, lhs, rhs) => try_evaluate_bin_op(op.clone(), lhs, rhs),
        _ => Ok(None),
    }
}

pub fn try_evaluate_unary_op(
    op: UnaryOp,
    expr: &Enriched<Expr<Ext>, Ext>,
) -> Result<Option<Value>> {
    let eval_expr = try_evaluate(expr)?;

    match (op, eval_expr) {
        (UnaryOp::Neg, Some(Value::Num(n))) => {
            let v =
                n.value()
                    .checked_neg()
                    .ok_or(TypecheckerError::EvaluationWillFailAtRuntime(Some(
                        expr.1.clone(),
                    )))?;
            Ok(Some(Value::Num(IntValue::Parsed(v))))
        }
        (UnaryOp::Not, Some(Value::Bool(b))) => Ok(Some(Value::Bool(!b))),
        _ => Ok(None),
    }
}

macro_rules! binary_op {
    ($lhs:expr, $rhs:expr, $op:tt, Bool) => {
        Ok(Some(Value::Bool($lhs $op $rhs)))
    };
    ($lhs:expr, $rhs:expr, $op:tt, Str) => {
        Ok(Some(Value::String(Box::new($lhs $op &$rhs))))
    };
    ($lhs:expr, $rhs:expr, repeat) => {
        Ok(Some(Value::String(Box::new($lhs.repeat($rhs as usize)))))
    };
}

macro_rules! safe_binary_op {
    ($lhs:expr, $rhs:expr, $op: ident) => {
        match paste! { $lhs.[<checked_$op>]($rhs) } {
            Some(v) => Ok(Some(Value::Num(IntValue::Parsed(v)))),
            None => Err(":("),
        }
    };
}

pub fn try_evaluate_bin_op(
    op: BinaryOp,
    lhs: &Enriched<Expr<Ext>, Ext>,
    rhs: &Enriched<Expr<Ext>, Ext>,
) -> Result<Option<Value>> {
    let eval_lhs = try_evaluate(lhs)?;
    let eval_rhs = try_evaluate(rhs)?;

    if let Some(eval_lhs) = eval_lhs {
        if let Some(eval_rhs) = eval_rhs {
            let res = match (op, eval_lhs, eval_rhs) {
                (BinaryOp::Add, Value::Num(l), Value::Num(r)) => {
                    safe_binary_op!(l.value(), r.value(), add)
                }
                (BinaryOp::Add, Value::String(l), Value::String(r)) => binary_op!(*l, *r, +, Str),
                (BinaryOp::Mul, Value::Num(l), Value::Num(r)) => {
                    safe_binary_op!(l.value(), r.value(), mul)
                }
                (BinaryOp::Mul, Value::String(l), Value::Num(r))
                | (BinaryOp::Mul, Value::Num(r), Value::String(l)) => {
                    binary_op!(*l, r.value(), repeat)
                }
                (BinaryOp::Sub, Value::Num(l), Value::Num(r)) => {
                    safe_binary_op!(l.value(), r.value(), sub)
                }
                (BinaryOp::Div, Value::Num(l), Value::Num(r)) => {
                    safe_binary_op!(l.value(), r.value(), div)
                }
                (BinaryOp::Mod, Value::Num(l), Value::Num(r)) => {
                    safe_binary_op!(l.value(), r.value(), rem)
                }
                (BinaryOp::Lt, Value::Num(l), Value::Num(r)) => {
                    binary_op!(l.value(), r.value(), <, Bool)
                }
                // (BinaryOp::Lt, Value::String(l), Value::String(r)) => binary_op!(l, r, <, Bool),
                (BinaryOp::Lte, Value::Num(l), Value::Num(r)) => {
                    binary_op!(l.value(), r.value(), <=, Bool)
                }
                // (BinaryOp::Lte, Value::String(l), Value::String(r)) => binary_op!(l, r, <=, Bool),
                (BinaryOp::Gt, Value::Num(l), Value::Num(r)) => {
                    binary_op!(l.value(), r.value(), >, Bool)
                }
                // (BinaryOp::Gt, Value::String(l), Value::String(r)) => binary_op!(l, r, >, Bool),
                (BinaryOp::Gte, Value::Num(l), Value::Num(r)) => {
                    binary_op!(l.value(), r.value(), >=, Bool)
                }
                // (BinaryOp::Gte, Value::String(l), Value::String(r)) => binary_op!(l, r, >=, Bool),
                (BinaryOp::Eq, l, r) => binary_op!(l, r, ==, Bool),
                (BinaryOp::Neq, l, r) => binary_op!(l, r, !=, Bool),
                (BinaryOp::And, Value::Bool(l), Value::Bool(r)) => binary_op!(l, r, &&, Bool),
                (BinaryOp::Or, Value::Bool(l), Value::Bool(r)) => binary_op!(l, r, ||, Bool),
                _ => Ok(None),
            };

            return res.or_else(|_| {
                bail!(TypecheckerError::EvaluationWillFailAtRuntime(Some(
                    lhs.1.clone()
                )))
            });
        }
    }

    Ok(None)
}

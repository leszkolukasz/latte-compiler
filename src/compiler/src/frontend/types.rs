use crate::common::LineCol;
use crate::frontend::error::TypecheckerError;
use crate::frontend::Context;
use crate::parser::Type;
use anyhow::{bail, Result};

pub fn check(context: &mut Context, typ: &Type, pos: &LineCol) -> Result<()> {
    match typ {
        Type::Void => bail!(TypecheckerError::InvalidVoidUsage(Some(pos.clone()))),
        Type::Fun(_, params) => params
            .iter()
            .map(|param| check(context, param, pos))
            .collect(),
        Type::Class { name, .. } => {
            if let Some(_) = context.get_class_info(name) {
                Ok(())
            } else {
                bail!(TypecheckerError::UndeclaredClass(
                    Some(pos.clone()),
                    *name.clone()
                ))
            }
        }
        Type::Array(t) => check(context, t, pos),
        Type::Str => {
            context.stats.is_string_used = true;
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn assert_one_of(actual: &Type, expected: &[Type], pos: LineCol) -> Result<()> {
    for exp in expected {
        if actual == exp {
            return Ok(());
        }
    }

    bail!(TypecheckerError::InvalidType(
        Some(pos.clone()),
        actual.clone(),
        expected.to_vec()
    ))
}

pub fn assert_one_of_tuples(
    actual: &(Type, Type),
    expected: &[(Type, Type)],
    pos: LineCol,
) -> Result<()> {
    for exp in expected {
        if actual == exp {
            return Ok(());
        }
    }

    bail!(TypecheckerError::InvalidBinOpType(
        Some(pos.clone()),
        actual.clone(),
        expected.to_vec()
    ))
}

pub fn assert_assignable(
    context: &mut Context,
    lhs: &Type,
    rval: &Type,
    is_lval: bool,
    pos: &LineCol,
) -> Result<()> {
    if !is_lval {
        bail!(TypecheckerError::NotAnLValue(Some(pos.clone())))
    }

    if !rval.is_assignable_to(lhs, context) {
        bail!(TypecheckerError::NotAssignable(
            Some(pos.clone()),
            rval.clone(),
            lhs.clone()
        ))
    }

    Ok(())
}

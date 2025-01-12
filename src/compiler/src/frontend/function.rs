use crate::common::{Enriched, LineCol};
use crate::frontend::error::TypecheckerError;
use crate::frontend::{block, keywords, types, Context, TypeInfo};
use crate::parser::Type::Class;
use crate::parser::{Routine, Type};
use crate::{Ext, NodeData};
use anyhow::{bail, Result};

// Potential return is used with `if` and `while` statements
// If `if` statement is of type `if, else` and all branches return, then the return type is CertainReturn
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ReturnInfo {
    // `NoReturn` includes case when no `return` statements were used (then attributes are None)
    // or were used inside block that is never executed e.g. `while(false) return x;`.
    NoReturn(Option<(LineCol, Box<Type>)>),
    // `Error` includes case when `error` builtin was called in the block (then attributes are None)
    // or when `error` was called and return statement appeared later.
    // Error(Option<(LineCol, Box<Type>)>),
    PotentialReturn(LineCol, Box<Type>),
    CertainReturn(LineCol, Box<Type>),
}

impl ReturnInfo {
    pub fn or(&self, other: &Self, context: &Context) -> Result<Self> {
        let common_data = self.get_common_data(other, context)?;

        match (self, other) {
            // (ReturnInfo::Error(_), _) => Ok(self.with_type(common_data)),
            // (_, ReturnInfo::Error(_)) => Ok(other.with_type(common_data)),
            (ReturnInfo::NoReturn(_), _) => Ok(other.with_type(common_data)),
            (_, ReturnInfo::NoReturn(_)) => Ok(self.with_type(common_data)),
            (ReturnInfo::CertainReturn(..), _) => Ok(self.with_type(common_data)),
            (_, ReturnInfo::CertainReturn(..)) => Ok(other.with_type(common_data)),
            _ => Ok(self.with_type(common_data)), // should be PotentialReturn
        }
    }

    pub fn and(&self, other: &Self, context: &Context) -> Result<Self> {
        let common_type = self.get_common_data(other, context)?;

        match (self, other) {
            (ReturnInfo::NoReturn(_), _) => Ok(other.weaken().with_type(common_type)),
            (_, ReturnInfo::NoReturn(_)) => Ok(self.weaken().with_type(common_type)),
            (ReturnInfo::PotentialReturn(..), _) => Ok(self.with_type(common_type)),
            (_, ReturnInfo::PotentialReturn(..)) => Ok(other.with_type(common_type)),
            _ => Ok(self.with_type(common_type)), // should be CertainReturn or Error
        }
    }

    pub fn get_common_data(
        &self,
        other: &Self,
        context: &Context,
    ) -> Result<Option<(LineCol, Box<Type>)>> {
        let (type1, type2) = (self.get_type(), other.get_type());
        let (pos1, pos2) = (self.get_pos(), other.get_pos());

        // Only merge types when both returns explicitly appeared in AST
        if pos1.is_ok() && pos2.is_ok() {
            match type1.merge(type2, context) {
                Some(merged_type) => Ok(Some((pos1?.clone(), Box::new(merged_type)))),
                None => bail!(TypecheckerError::ConflictingReturnType {
                    prev_pos: Some(pos1?.clone()),
                    prev_typ: type1.clone(),
                    pos: Some(pos2?.clone()),
                    typ: type2.clone(),
                }),
            }
        } else if pos1.is_ok() {
            return Ok(Some((pos1?.clone(), Box::new(type1.clone()))));
        } else if pos2.is_ok() {
            return Ok(Some((pos2?.clone(), Box::new(type2.clone()))));
        } else {
            Ok(None)
        }
    }

    // If return is of type `CertainReturn` and some branch does not return
    // then return is no longer certain.
    pub fn weaken(&self) -> Self {
        match self {
            ReturnInfo::CertainReturn(pos, t) => {
                ReturnInfo::PotentialReturn(pos.clone(), t.clone())
            }
            _ => self.clone(),
        }
    }

    pub fn get_type(&self) -> &Type {
        match self {
            ReturnInfo::NoReturn(s) => match s {
                Some((_, t)) => t,
                None => &Type::Void,
            },
            // ReturnInfo::Error(s) => match s {
            //     Some((_, t)) => t,
            //     None => &Type::Any
            // },
            ReturnInfo::PotentialReturn(_, t) => t,
            ReturnInfo::CertainReturn(_, t) => t,
        }
    }

    pub fn get_pos(&self) -> Result<&LineCol> {
        match self {
            ReturnInfo::NoReturn(s) => match s {
                Some(pos) => Ok(&pos.0),
                None => bail!("Cannot extract position"),
            },
            // ReturnInfo::Error(s) => match s {
            //     Some(pos) => Ok(&pos.0),
            //     None => bail!("Cannot extract position"),
            // },
            ReturnInfo::PotentialReturn(pos, _) => Ok(pos),
            ReturnInfo::CertainReturn(pos, _) => Ok(pos),
        }
    }

    pub fn with_type(&self, data: Option<(LineCol, Box<Type>)>) -> Self {
        match self {
            ReturnInfo::NoReturn(_) => ReturnInfo::NoReturn(data),
            // ReturnInfo::Error(_) => ReturnInfo::Error(data),
            ReturnInfo::PotentialReturn(_, _) => {
                // `data` should not be None
                let data = data.unwrap();
                ReturnInfo::PotentialReturn(data.0, data.1)
            }
            ReturnInfo::CertainReturn(_, _) => {
                // `data` should not be None
                let data = data.unwrap();
                ReturnInfo::CertainReturn(data.0, data.1)
            }
        }
    }
}

pub fn check_definition(
    context: &mut Context,
    fun: &mut Enriched<&mut Routine<Ext>, Ext>,
) -> Result<()> {
    keywords::check(context, &fun.0.name, &fun.1)?;

    block::on_enter(context);
    check_params(context, fun)?;
    block::on_exit(context);

    let fun_ret_type = &*fun.0.return_type;

    if fun_ret_type != &Type::Void {
        types::check(context, fun_ret_type, &fun.1)?;
    }

    Ok(())
}

pub fn check_body(context: &mut Context, fun: &mut Enriched<&mut Routine<Ext>, Ext>) -> Result<()> {
    block::on_enter(context);

    check_params(context, fun)?;
    let ast = &mut fun.0;
    let ret_info = block::check(context, &mut ast.body)?;
    check_ret_type(context, fun, &ret_info)?;

    block::on_exit(context);

    Ok(())
}

pub fn check_params(
    context: &mut Context,
    fun: &mut Enriched<&mut Routine<Ext>, Ext>,
) -> Result<()> {
    let ast = &fun.0;
    let params = &*ast.params;

    if let Some(cls) = &context.this {
        context.add_to_scope(
            "self",
            &TypeInfo {
                typ: Class {
                    name: Box::new(cls.clone()),
                    info: None,
                },
                pos: None,
                is_global: false,
            },
        );
    }

    for param in params {
        let param_name = &*param.0 .1;
        let param_type = &*param.0 .0;

        keywords::check(context, param_name, &fun.1)?;
        types::check(context, param_type, &param.1)?;

        if context.is_redeclaration(param_name) {
            bail!(TypecheckerError::RepeatedParamName(
                Some(param.1.clone()),
                param_name.clone()
            ))
        } else {
            context.add_to_scope(
                &param_name,
                &TypeInfo {
                    typ: param_type.clone(),
                    pos: Some(param.1.clone()),
                    is_global: false,
                },
            );
        }
    }

    Ok(())
}

pub fn check_ret_type(
    context: &mut Context,
    fun: &mut Enriched<&mut Routine<Ext>, Ext>,
    ret_info: &ReturnInfo,
) -> Result<()> {
    let ast = &mut fun.0;
    let fun_ret_type = &*ast.return_type;

    if let ReturnInfo::NoReturn(_) = ret_info {
        if fun_ret_type != &Type::Void {
            bail!(TypecheckerError::MissingReturn(
                Some(fun.1.clone()),
                *fun.0.name.clone()
            ))
        }

        // Add explicit return statement for void functions
        ast.body.push(
            Box::new((
                crate::parser::Stmt::ReturnVoid,
                fun.1.clone(),
                NodeData {
                    expr_typ: Some(Type::Void),
                },
            )), // pos is not correct, but it does not matter at this point
        )
    }

    if !ret_info.get_type().is_assignable_to(fun_ret_type, context) {
        bail!(TypecheckerError::ConflictingReturnType {
            pos: Some(fun.1.clone()),
            typ: fun_ret_type.clone(),
            prev_pos: ret_info.get_pos().map_or(None, |r| Some(r.clone())),
            prev_typ: ret_info.get_type().clone(),
        })
    }

    if let ReturnInfo::PotentialReturn(_, _) = ret_info {
        if fun_ret_type != &Type::Void {
            bail!(TypecheckerError::PotentiallyMissingReturn(
                Some(fun.1.clone()),
                *fun.0.name.clone()
            ))
        }
    }

    Ok(())
}

pub fn build_function_type(fun: &Routine<Ext>) -> Type {
    let ret_type = &fun.return_type;
    let params = &fun.params;
    let param_types = params
        .iter()
        .map(|param| param.0 .0.clone())
        .collect::<Vec<_>>();

    Type::Fun(ret_type.clone(), Box::new(param_types))
}

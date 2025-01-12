use crate::frontend::function::ReturnInfo;
use crate::frontend::{statement, Context};
use crate::parser::Block;
use crate::Ext;
use anyhow::Result;
use std::collections::HashSet;

pub fn check(context: &mut Context, block: &mut Block<Ext>) -> Result<ReturnInfo> {
    on_enter(context);

    let (dead_code_start, ret_type) = check_stms(context, block)?;
    eliminate_dead_code(block, dead_code_start);

    on_exit(context);

    Ok(ret_type)
}

pub fn check_stms(
    context: &mut Context,
    block: &mut Block<Ext>,
) -> Result<(Option<usize>, ReturnInfo)> {
    block.iter_mut().enumerate().try_fold(
        (None, ReturnInfo::NoReturn(None)),
        |(dead_code_start, prev_ret), (idx, stmt_ast)| {
            let ret = statement::check(context, stmt_ast)?;
            let merged_ret_type = prev_ret.or(&ret, context)?;

            match merged_ret_type {
                ReturnInfo::CertainReturn(..) if dead_code_start.is_none() => {
                    Ok((Some(idx), merged_ret_type))
                }
                _ => Ok((dead_code_start, merged_ret_type)),
            }
        },
    )
}

fn eliminate_dead_code(block: &mut Block<Ext>, dead_code_start: Option<usize>) {
    if let Some(start) = dead_code_start {
        block.truncate(start + 1);
    }
}

pub fn on_enter(context: &mut Context) {
    context.new_names.push(HashSet::new());
}

pub fn on_exit(context: &mut Context) {
    let new_names = context.new_names.pop().unwrap();

    for name in new_names {
        context.remove_from_scope(&name);
    }
}

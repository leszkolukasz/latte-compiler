use crate::backend::{statement, Context, ASM};
use crate::parser::Block;
use crate::Ext;
use anyhow::Result;
use std::collections::HashSet;

pub fn compile(context: &mut Context, block: &Block<Ext>) -> Result<ASM> {
    on_enter(context);

    let asm = block.iter().try_fold(ASM::new(), |mut asm, stmt| {
        let mut stmt_asm = statement::compile(context, stmt)?;
        asm.append(&mut stmt_asm);

        Ok::<_, anyhow::Error>(asm)
    })?;

    on_exit(context);

    Ok(asm)
}


pub fn on_enter(context: &mut Context) {
    context.new_vars.push(HashSet::new());
}

pub fn on_exit(context: &mut Context) {
    let new_vars = context.new_vars.pop().unwrap();

    for var in new_vars {
        context.remove_var(&var);
    }
}
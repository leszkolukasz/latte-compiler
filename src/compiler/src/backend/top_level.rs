use crate::backend::function;
use crate::backend::{asm, class, Context, ASM};
use crate::parser::{Program, TopDef};
use crate::Ext;
use anyhow::Result;

pub const EXTERNAL_FUNCTIONS: [&str; 6] = [
    "error",
    "printInt",
    "printString",
    "readInt",
    "readString",
    "printChar",
];

pub fn compile(context: &mut Context, program: &Program<Ext>) -> Result<ASM> {
    let mut asm = ASM::new();

    asm.push_back(asm::CUSTOM("\n\t.section .text".to_string()));

    for decl in program.iter() {
        let (def, pos, t) = &**decl;
        match def {
            TopDef::Class(c) => asm.append(&mut class::compile_text(
                context,
                &(c, pos.clone(), t.clone()),
            )?),
            TopDef::Fun(r) => asm.append(&mut function::compile(
                context,
                &(r, pos.clone(), t.clone()),
            )?),
        }
    }

    asm.push_back(asm::CUSTOM("\n\t.section .data".to_string()));

    for decl in program.iter() {
        let (def, pos, t) = &**decl;
        match def {
            TopDef::Class(c) => asm.append(&mut class::compile_data(
                context,
                &(c, pos.clone(), t.clone()),
            )?),
            _ => {}
        }
    }

    for (str, label) in context.string_labels.iter() {
        asm.push_back(asm::CUSTOM("".to_string())); // for newline
        asm.push_back(asm::LABEL(label.clone()));
        asm.push_back(asm::CUSTOM(format!("\t.string \"{}\"", str)));
    }

    for used_function in context.used_functions.iter() {
        if EXTERNAL_FUNCTIONS.contains(&used_function.as_str()) {
            asm.push_front(asm::CUSTOM(format!("\t.extern {}", used_function)));
        }
    }

    asm.push_front(asm::CUSTOM("\t.globl String_vtable".to_string()));
    asm.push_front(asm::CUSTOM("\t.globl main".to_string()));

    Ok(asm)
}

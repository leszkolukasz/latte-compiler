use crate::backend::{asm, function, Context, ASM};
use crate::common::Enriched;
use crate::parser::{Class, ClassMember};
use crate::Ext;
use anyhow::Result;

pub fn compile_text(context: &mut Context, class: &Enriched<&Class<Ext>, Ext>) -> Result<ASM> {
    let mut asm = ASM::new();
    let class_name = &*class.0.name;

    if class_name == "String" && !context.stats.is_string_used {
        return Ok(asm);
    }

    asm.push_back(asm::CUSTOM("".to_string())); // for newline
    context.class_name = Some(class_name.clone());

    for member in class.0.members.iter() {
        match &member.0 {
            ClassMember::Method(r) => {
                asm.append(&mut function::compile(
                    context,
                    &(r, class.1.clone(), member.2.clone()),
                )?);
            }
            _ => {}
        }
    }

    context.class_name = None;

    Ok(asm)
}

pub fn compile_data(context: &mut Context, class: &Enriched<&Class<Ext>, Ext>) -> Result<ASM> {
    let mut asm = ASM::new();

    let class_name = &*class.0.name;
    let info = context.class_info.get(class_name).unwrap();

    if class_name == "String" && !context.stats.is_string_used {
        // runtime.c needs String_vtable label to be defined
        asm.push_back(asm::LABEL(format!("\n{}_vtable", *class_name)));
        return Ok(asm);
    }

    if !info.vtable.is_empty() {
        asm.push_back(asm::LABEL(format!("\n{}_vtable", *class_name)));
        for (cls, name) in &info.vtable {
            asm.push_back(asm::CUSTOM(format!("\t.quad {}${}", *cls, name)));
        }
    }

    Ok(asm)
}

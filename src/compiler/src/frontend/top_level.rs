use crate::common::Enriched;
use crate::frontend::error::TypecheckerError;
use crate::frontend::{block, class, function, keywords, Context, TypeInfo};
use crate::parser::{Class, Program, Routine, TopDef, Type};
use crate::Ext;
use anyhow::{bail, Result};
use std::collections::HashMap;

pub fn check(context: &mut Context, program: &mut Program<Ext>) -> Result<()> {
    block::on_enter(context);

    let mut class_graph: HashMap<String, String> = HashMap::new();
    let found_main = add_top_level_definitions(context, program, &mut class_graph)?;

    if !found_main {
        bail!(TypecheckerError::MainNotFound);
    }

    check_top_level_definitions(context, program)?;
    check_class_hierarchy(context, &class_graph)?;
    check_bodies(context, program)?;

    block::on_exit(context);

    Ok(())
}
pub fn add_top_level_definitions(
    context: &mut Context,
    program: &mut Program<Ext>,
    class_graph: &mut HashMap<String, String>,
) -> Result<bool> {
    let mut found = false;

    for decl in program.iter_mut() {
        let (def, pos, t) = &mut **decl;

        match def {
            TopDef::Fun(r) => {
                found = add_function_definition(context, &(r, pos.clone(), t.clone()))? || found
            }
            TopDef::Class(c) => {
                add_class_definition(context, &(c, pos.clone(), t.clone()), class_graph)?
            }
        }
    }

    Ok(found)
}

pub fn add_function_definition(
    context: &mut Context,
    fun: &Enriched<&Routine<Ext>, Ext>,
) -> Result<bool> {
    let main_type = Type::Fun(Box::new(Type::Int), Box::new(vec![]));

    let ast = fun.0;
    let name = &*ast.name;
    let fun_type = function::build_function_type(fun.0);

    keywords::check(context, &name, &fun.1)?;

    if context.reserved_function_names.contains(name.as_str()) {
        bail!(TypecheckerError::CannotRedeclareFunction(
            Some(fun.1.clone()),
            name.clone()
        ));
    }

    if context.is_redeclaration(&name) {
        let first_declaration = context.get_from_scope(&name, &fun.1)?.unwrap();
        bail!(TypecheckerError::RepeatedDeclaration {
            prev_pos: first_declaration.pos.clone(),
            pos: Some(fun.1.clone()),
            name: name.clone(),
        });
    }

    context.add_to_scope(
        name,
        &TypeInfo {
            typ: fun_type.clone(),
            pos: Some(fun.1.clone()),
            is_global: true,
        },
    );

    if name == "main" {
        if fun_type != main_type {
            bail!(TypecheckerError::InvalidType(
                Some(fun.1.clone()),
                fun_type,
                vec![main_type]
            ))
        }

        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn add_class_definition(
    context: &mut Context,
    class: &Enriched<&Class<Ext>, Ext>,
    class_graph: &mut HashMap<String, String>,
) -> Result<()> {
    let ast = class.0;
    let name = &*ast.name;
    let class_type = class::build_class_type(class);

    keywords::check(context, name, &class.1)?;

    if context.is_redeclaration(&name) {
        let first_declaration = context.get_from_scope(&name, &class.1)?.unwrap();
        bail!(TypecheckerError::RepeatedDeclaration {
            prev_pos: first_declaration.pos.clone(),
            pos: Some(class.1.clone()),
            name: name.clone(),
        });
    }

    context.add_to_scope(
        name,
        &TypeInfo {
            typ: class_type.clone(),
            pos: Some(class.1.clone()),
            is_global: true,
        },
    );

    if let Some(s) = &ast.super_cls {
        keywords::check(context, s, &class.1)?;
        class_graph.insert(name.clone(), s.to_string());
    }

    Ok(())
}

pub fn check_top_level_definitions(
    context: &mut Context,
    program: &mut Program<Ext>,
) -> Result<()> {
    for decl in program.iter_mut() {
        let (def, pos, t) = &mut **decl;

        match def {
            TopDef::Fun(r) => {
                function::check_definition(context, &mut (r, pos.clone(), t.clone()))?
            }
            TopDef::Class(c) => class::check_definition(context, &mut (c, pos.clone(), t.clone()))?,
        }
    }

    Ok(())
}

pub fn check_class_hierarchy(
    context: &mut Context,
    class_graph: &HashMap<String, String>,
) -> Result<()> {
    let mut visited: HashMap<String, i64> = HashMap::new();

    // Check for cycles
    for (name, _) in class_graph {
        if visited.get(&name.clone()).unwrap_or(&0) == &0 {
            let mut path = vec![];
            dfs(context, class_graph, &name, &mut visited, &mut path)?;
        }
    }

    Ok(())
}

pub fn dfs(
    context: &Context,
    class_graph: &HashMap<String, String>,
    name: &str,
    visited: &mut HashMap<String, i64>,
    path: &mut Vec<String>,
) -> Result<()> {
    path.push(name.to_string());
    visited.insert(name.to_string(), 1);

    let next = class_graph.get(name);
    if next.is_some() {
        let next = next.unwrap();

        let status = visited.get(&next.clone()).unwrap_or(&0);
        if status == &1 {
            path.push(next.to_string());
            bail!(TypecheckerError::CyclicInheritance(path.clone()))
        } else if status == &0 {
            dfs(context, class_graph, next, visited, path)?;
        }
    }

    path.pop();
    visited.insert(name.to_string(), 2);

    Ok(())
}

pub fn check_bodies(context: &mut Context, program: &mut Program<Ext>) -> Result<()> {
    for decl in program.iter_mut() {
        let (def, pos, t) = &mut **decl;
        match def {
            // t should not be cloned, but it does not matter as it is used only for expressions as of now
            TopDef::Fun(r) => function::check_body(context, &mut (r, pos.clone(), t.clone()))?,
            TopDef::Class(c) => class::check_body(context, &mut (c, pos.clone(), t.clone()))?,
        }
    }

    Ok(())
}

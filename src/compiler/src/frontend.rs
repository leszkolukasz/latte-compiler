pub mod block;
pub mod class;
pub mod error;
pub mod expression;
pub mod function;
pub mod keywords;
pub mod statement;
pub mod top_level;
pub mod types;

use crate::common::LineCol;
use crate::parser::{ClassInfo, Program, Type};
use crate::Ext;
use anyhow::Result;
use maplit::{hashmap, hashset};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct TypeInfo {
    pub typ: Type,
    pos: Option<LineCol>,
    is_global: bool,
}

#[derive(Debug)]
pub struct Context {
    pub type_env: HashMap<String, Vec<TypeInfo>>,
    new_names: Vec<HashSet<String>>, // List of names declared in each block. Popped on block exit.
    this: Option<String>,            // Current class name.
    reserved_keywords: HashSet<&'static str>,
    reserved_function_names: HashSet<&'static str>,
    stats: Stats,
}

#[derive(Debug)]
pub struct Stats {
    pub is_string_used: bool,
}

impl Context {
    pub fn new() -> Self {
        Context {
            new_names: Vec::new(),
            this: None,
            stats: Stats {
                is_string_used: false,
            },
            reserved_keywords: hashset! { "self" },
            reserved_function_names: hashset! { "printInt", "printString", "error", "readInt", "readString" },
            type_env: hashmap! {
                "printInt".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Void), Box::new(vec![Box::new(Type::Int)])),
                    pos: None,
                    is_global: true,
                }],
                "printString".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Void), Box::new(vec![Box::new(Type::Str)])),
                    pos: None,
                    is_global: true,
                }],
                "printChar".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Void), Box::new(vec![Box::new(Type::Bool)])),
                    pos: None,
                    is_global: true,
                }],
                "error".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Void), Box::new(vec![])),
                    pos: None,
                    is_global: true,
                }],
                "readInt".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Int), Box::new(vec![])),
                    pos: None,
                    is_global: true,
                }],
                "readString".into() => vec![TypeInfo {
                    typ: Type::Fun(Box::new(Type::Str), Box::new(vec![])),
                    pos: None,
                    is_global: true,
                }],
            },
        }
    }

    pub fn get_from_scope(&self, name: &str, pos: &LineCol) -> Result<Option<&TypeInfo>> {
        let declarations = self.type_env.get(name);
        let mut res = None;

        if let Some(declaration) = declarations.map_or(None, |v| {
            // Declaration could be global function so we should skip it for now.
            // At least now, there can be only one global function with the same name.
            if v.last().is_some() && v.last().unwrap().is_global {
                None
            } else {
                v.last()
            }
        }) {
            return Ok(Some(declaration));
        }

        if let Some(this) = &self.this {
            res = class::find_class_member(self, &this, name, pos)?;
        }

        if let Some(decl) = res {
            Ok(Some(decl))
        } else if let Some(declaration) = declarations.map_or(None, |v| v.last()) {
            // Return previously skipped global function
            assert!(declaration.is_global);
            Ok(Some(declaration))
        } else {
            Ok(None)
        }
    }

    pub fn get_class_info(&self, name: &str) -> &Option<ClassInfo> {
        let declarations = self.type_env.get(name);

        // Assumes: no nested classes
        if let Some(declarations) = declarations {
            for decl in declarations {
                if let Type::Class {
                    info: class_info, ..
                } = &decl.typ
                {
                    // At this point class_info should be Some, if not it means we found
                    // variable of class type.
                    assert!(class_info.is_some());
                    return class_info;
                }
            }
        }
        &None
    }

    pub fn add_to_scope(&mut self, name: &str, typ: &TypeInfo) {
        if !self.type_env.contains_key(name) {
            self.type_env.insert(name.into(), vec![]);
        }

        let declarations = self.type_env.get_mut(name).unwrap();
        declarations.push(typ.clone());

        self.new_names.last_mut().unwrap().insert(name.into());
    }

    pub fn remove_from_scope(&mut self, name: &str) {
        let declarations = self.type_env.get_mut(name).unwrap();
        declarations.pop().unwrap();
    }

    pub fn is_redeclaration(&self, name: &str) -> bool {
        let new_names = self.new_names.last().unwrap();
        new_names.contains(name)
    }
}

pub fn check(program: &mut Program<Ext>) -> Result<Stats> {
    let mut context = Context::new();
    top_level::check(&mut context, program)?;

    Ok(context.stats)
}

use crate::common::{Enriched, LineCol};
use crate::frontend::error::TypecheckerError;
use crate::frontend::{block, function, keywords, types, Context, TypeInfo};
use crate::parser::{Class, ClassInfo, ClassMember, Type};
use crate::Ext;
use anyhow::{bail, Result};
use std::cmp::min;
use std::collections::{HashMap, HashSet};

pub fn check_definition(
    context: &mut Context,
    class: &mut Enriched<&mut Class<Ext>, Ext>,
) -> Result<()> {
    let ast = &mut class.0;

    keywords::check(context, &ast.name, &class.1)?;

    if let Some(s) = &ast.super_cls {
        keywords::check(context, &s, &class.1)?;

        if context.get_class_info(s).is_none() {
            bail!(TypecheckerError::UndeclaredClass(
                Some(class.1.clone()),
                *s.clone()
            ))
        }
    }

    on_enter(context, ast);
    check_member_definitions(context, ast)?;
    on_exit(context);

    Ok(())
}

pub fn check_body(context: &mut Context, class: &mut Enriched<&mut Class<Ext>, Ext>) -> Result<()> {
    let ast = &mut class.0;

    on_enter(context, ast);
    check_member_bodies(context, ast)?;
    on_exit(context);

    Ok(())
}

pub fn check_member_definitions(context: &mut Context, class: &mut Class<Ext>) -> Result<()> {
    let mut member_names: HashSet<String> = HashSet::new();

    for member in class.members.iter() {
        let member_name: &String = member.0.get_name();
        keywords::check(context, &member_name, &member.1)?;

        if member_names.contains(member_name) {
            bail!(TypecheckerError::RepeatedMemberName(
                Some(member.1.clone()),
                member_name.clone()
            ))
        }

        member_names.insert(member_name.clone());
    }

    for member in class.members.iter_mut() {
        match &mut member.0 {
            ClassMember::Field { typ, .. } => types::check(context, &typ, &member.1)?,
            ClassMember::Method(r) => {
                function::check_definition(context, &mut (r, member.1.clone(), member.2.clone()))?;

                // Check for overloading
                if let Some(s) = &class.super_cls {
                    if let Some(super_fun) = find_class_member(context, s, &r.name, &member.1)? {
                        let fun_typ =
                            find_class_member(context, &class.name, &r.name, &member.1)?.unwrap();

                        if fun_typ.typ != super_fun.typ && super_fun.typ.is_fun() {
                            bail!(TypecheckerError::OverloadingNotSupported {
                                pos: Some(member.1.clone()),
                                class_name: *class.name.clone(),
                                fun_name: *r.name.clone(),
                                fun_type: fun_typ.typ.clone(),
                                super_fun_type: super_fun.typ.clone(),
                                superclass_name: *s.clone()
                            })
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

pub fn check_member_bodies(context: &mut Context, class: &mut Class<Ext>) -> Result<()> {
    for member in class.members.iter_mut() {
        match &mut member.0 {
            ClassMember::Method(r) => {
                function::check_body(context, &mut (r, member.1.clone(), member.2.clone()))?
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn build_class_type(class: &Enriched<&Class<Ext>, Ext>) -> Type {
    let ast = class.0;
    let mut members: HashMap<String, TypeInfo> = HashMap::new();

    for member in ast.members.iter() {
        match &member.0 {
            ClassMember::Field { name, typ } => {
                members.insert(
                    *name.clone(),
                    TypeInfo {
                        typ: *typ.clone(),
                        pos: Some(member.1.clone()),
                        is_global: false,
                    },
                );
            }
            ClassMember::Method(r) => {
                members.insert(
                    *r.name.clone(),
                    TypeInfo {
                        typ: function::build_function_type(&r),
                        pos: Some(member.1.clone()),
                        is_global: false,
                    },
                );
            }
        }
    }

    Type::Class {
        name: ast.name.clone(),
        info: Some(ClassInfo {
            inherits: ast.super_cls.clone(),
            members: Box::new(members),
        }),
    }
}

pub fn find_class_member<'a>(
    context: &'a Context,
    class_name: &str,
    member_name: &str,
    pos: &LineCol,
) -> Result<Option<&'a TypeInfo>> {
    if let Some(info) = context.get_class_info(class_name) {
        if info.members.contains_key(member_name) {
            Ok(Some(&info.members.get(member_name).unwrap()))
        } else if let Some(super_cls) = &info.inherits {
            find_class_member(context, &super_cls, member_name, pos)
        } else {
            Ok(None)
        }
    } else {
        bail!(TypecheckerError::UndeclaredClass(
            Some(pos.clone()),
            class_name.into()
        ))
    }
}

pub fn is_subclass(
    context: &Context,
    subclass: &(String, &ClassInfo),
    class: &(String, &ClassInfo),
) -> bool {
    if subclass.0 == class.0 {
        return true;
    }

    if let Some(super_cls) = &subclass.1.inherits {
        let super_cls_info = context.get_class_info(super_cls);
        if let Some(super_cls_info) = super_cls_info {
            return is_subclass(context, &(*super_cls.clone(), super_cls_info), class);
        }
    }

    false
}

pub fn lca(context: &Context, class_a: &String, class_b: &String) -> Option<String> {
    let mut path_a: Vec<String> = vec![];
    let mut path_b: Vec<String> = vec![];

    let mut class_a = class_a.clone();
    let mut class_b = class_b.clone();

    while let Some(info) = context.get_class_info(&class_a) {
        path_a.push(class_a.clone());

        if let Some(inherits) = &info.inherits {
            class_a = *inherits.clone();
        } else {
            break;
        }
    }

    while let Some(info) = context.get_class_info(&class_b) {
        path_b.push(class_b.clone());

        if let Some(inherits) = &info.inherits {
            class_b = *inherits.clone();
        } else {
            break;
        }
    }

    path_a = path_a.into_iter().rev().collect::<Vec<String>>();
    path_b = path_b.into_iter().rev().collect::<Vec<String>>();

    let mut idx: i64 = -1;
    for i in 0..min(path_a.len(), path_b.len()) {
        if path_a[i] != path_b[i] {
            break;
        }

        idx = i as i64;
    }

    if idx == -1 {
        None
    } else {
        Some(path_a[idx as usize].clone())
    }
}

fn on_enter(context: &mut Context, class: &Class<Ext>) {
    block::on_enter(context);
    context.this = Some(*class.name.clone());
}

fn on_exit(context: &mut Context) {
    block::on_exit(context);
    context.this = None;
}

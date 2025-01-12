use crate::common::{Enriched, LineCol, PtrList};
use crate::frontend::{class, Context, TypeInfo};
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IntValue {
    Raw(Box<String>),
    Parsed(i64),
}

impl IntValue {
    pub fn value(&self) -> i64 {
        match self {
            IntValue::Raw(v) => v.parse::<i64>().unwrap(),
            IntValue::Parsed(v) => *v,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Num(IntValue),
    String(Box<String>),
    Bool(bool),
    Null,
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Num(_) => Type::Int,
            Value::String(_) => Type::Str,
            Value::Bool(_) => Type::Bool,
            Value::Null => Type::MetaClass,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    And,
    Or,
}

impl BinaryOp {
    // Returns tuples of (lhs type, rhs type, result type)
    pub fn get_type_combo(&self) -> Vec<(Type, Type, Type)> {
        match self {
            BinaryOp::Add => vec![
                (Type::Int, Type::Int, Type::Int),
                (Type::Str, Type::Str, Type::Str),
            ],
            BinaryOp::Mul => vec![
                (Type::Int, Type::Int, Type::Int),
                (Type::Str, Type::Int, Type::Str),
                (Type::Int, Type::Str, Type::Str),
            ],
            BinaryOp::Sub | BinaryOp::Div | BinaryOp::Mod => {
                vec![(Type::Int, Type::Int, Type::Int)]
            }
            BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => vec![
                (Type::Int, Type::Int, Type::Bool),
                // (Type::Str, Type::Str, Type::Bool),
            ],
            BinaryOp::Eq | BinaryOp::Neq => vec![
                (Type::Int, Type::Int, Type::Bool),
                (Type::Str, Type::Str, Type::Bool),
                (Type::Bool, Type::Bool, Type::Bool),
                (Type::MetaClass, Type::MetaClass, Type::Bool),
            ],
            BinaryOp::And | BinaryOp::Or => vec![(Type::Bool, Type::Bool, Type::Bool)],
        }
    }

    pub fn reverse_direction(&self) -> Self {
        match self {
            BinaryOp::Lt => BinaryOp::Gt,
            BinaryOp::Lte => BinaryOp::Gte,
            BinaryOp::Gt => BinaryOp::Lt,
            BinaryOp::Gte => BinaryOp::Lte,
            op => op.clone(),
        }
    }

    pub fn can_swap(&self) -> bool {
        match self {
            BinaryOp::Add => true,
            BinaryOp::Mul => true,
            BinaryOp::Eq => true,
            BinaryOp::Neq => true,
            BinaryOp::Lte => true,
            BinaryOp::Gte => true,
            BinaryOp::Lt => true,
            BinaryOp::Gt => true,
            BinaryOp::Sub => false,
            BinaryOp::Div => false,
            BinaryOp::Mod => false,
            BinaryOp::And => false,
            BinaryOp::Or => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub inherits: Option<Box<String>>,
    pub members: Box<HashMap<String, TypeInfo>>,
}

impl PartialEq for ClassInfo {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Eq for ClassInfo {}

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Int,
    Str,
    Bool,
    Void,
    Array(Box<Self>),
    // MetaArray, // any array is of this type
    Fun(Box<Self>, Box<PtrList<Self>>), // return type, parameter types
    Class {
        name: Box<String>,
        info: Option<ClassInfo>, // stored only for class definition
    },
    MetaClass, // any class is of this type
    Any,
}

impl Type {
    pub fn get_array_item_type(&self) -> Option<Type> {
        match self {
            Type::Array(t) => Some(*t.clone()),
            Type::Str => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn is_fun(&self) -> bool {
        match self {
            Type::Fun(..) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Type::Array(..) => true,
            _ => false,
        }
    }

    // is `self` assignable to `other`
    pub fn is_assignable_to(&self, other: &Self, context: &mut Context) -> bool {
        match (other, self) {
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::Int, Type::Int) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Void, Type::Void) => true,
            (Type::Array(a), Type::Array(b)) => b.is_assignable_to(a, context),
            (Type::MetaClass, Type::MetaClass) => true,
            (Type::MetaClass, Type::Class { .. }) => true,
            (Type::Class { .. }, Type::MetaClass) => true,
            (Type::Class { name: name1, .. }, Type::Class { name: name2, .. }) => {
                let linfo = context.get_class_info(name1);
                let rinfo = context.get_class_info(name2);

                if let Some(linfo) = linfo {
                    if let Some(rinfo) = rinfo {
                        class::is_subclass(
                            context,
                            &(*name2.clone(), rinfo),
                            &(*name1.clone(), linfo),
                        )
                    } else {
                        panic!("No info found for class. This should not happen.")
                    }
                } else {
                    panic!("No info found for class. This should not happen.")
                }
            }
            (Type::Fun(..), Type::Fun(..)) => {
                // This comparison would be useful for overriding, right now it shouldn't be possible to override
                false
            }
            _ => false,
        }
    }

    // Tries to merge two types. For classes this means finding LCA, similarly for arrays.
    pub fn merge(&self, other: &Self, context: &Context) -> Option<Type> {
        match (self, other) {
            (Type::Any, _) => Some(other.clone()),
            (_, Type::Any) => Some(self.clone()),
            (Type::Int, Type::Int) => Some(Type::Int),
            (Type::Str, Type::Str) => Some(Type::Str),
            (Type::Bool, Type::Bool) => Some(Type::Bool),
            (Type::Void, Type::Void) => Some(Type::Void),
            (Type::MetaClass, Type::MetaClass) => Some(self.clone()),
            (Type::MetaClass, Type::Class { .. }) => Some(other.clone()),
            (Type::Class { .. }, Type::MetaClass) => Some(self.clone()),
            (Type::Array(a), Type::Array(b)) => a
                .merge(b, context)
                .map_or(None, |t| Some(Type::Array(Box::new(t)))),
            (Type::Class { name: name1, .. }, Type::Class { name: name2, .. }) => {
                if let Some(cls_name) = class::lca(context, name1, name2) {
                    Some(Type::Class {
                        name: Box::new(cls_name),
                        info: None,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            Type::Int => 8,
            Type::Str => 8,
            Type::Bool => 1, // TODO: support 1 byte for bool
            Type::Array(_) => 8,
            Type::Class { .. } => 8,
            _ => unreachable!(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Int => write!(f, "int"),
            Type::Str => write!(f, "str"),
            Type::Bool => write!(f, "bool/char"),
            Type::Void => write!(f, "void"),
            Type::Array(t) => write!(f, "{}[]", t),
            Type::Class { name, .. } => write!(f, "<Class {}>", name),
            Type::Fun(ret, params) => {
                let params_str = params
                    .iter()
                    .map(|p| format!("{}", p)) // Format each parameter type
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({}) -> {}", params_str, ret)
            }
            Type::MetaClass => write!(f, "<ClassType>",),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Void, Type::Void) => true,
            (Type::Array(a), Type::Array(b)) => a == b,
            (Type::Class { name: name1, .. }, Type::Class { name: name2, .. }) => name1 == name2,
            (Type::MetaClass, Type::MetaClass) => true,
            (Type::MetaClass, Type::Class { .. }) => true,
            (Type::Class { .. }, Type::MetaClass) => true,
            (Type::Fun(ret_type1, params1), Type::Fun(ret_type2, params2)) => {
                ret_type1 == ret_type2 && params1 == params2
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Routine<D: Default + 'static = ()> {
    pub name: Box<String>,
    pub return_type: Box<Type>,
    pub params: Box<PtrList<Enriched<Arg, D>>>,
    pub body: Box<Block<D>>,
}

#[derive(Debug)]
pub struct Class<D: Default + 'static = ()> {
    pub name: Box<String>,
    pub super_cls: Option<Box<String>>,
    pub members: Box<PtrList<Enriched<ClassMember<D>, D>>>,
}

#[derive(Debug)]
pub enum TopDef<D: Default + 'static = ()> {
    Fun(Routine<D>),
    Class(Class<D>),
}

#[derive(Debug)]
pub enum ClassMember<D: Default + 'static = ()> {
    Field { typ: Box<Type>, name: Box<String> },
    Method(Routine<D>),
}

impl<D: Default + 'static> ClassMember<D> {
    pub fn get_name(&self) -> &String {
        match self {
            ClassMember::Field { name, .. } => name,
            ClassMember::Method(r) => &r.name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<D: Default + 'static = ()> {
    Ident(Box<String>),
    Literal(Box<Value>),
    Unary(UnaryOp, Box<Enriched<Self, D>>),
    Binary(BinaryOp, Box<Enriched<Self, D>>, Box<Enriched<Self, D>>),
    Apply(Box<Enriched<Self, D>>, Box<PtrList<Enriched<Self, D>>>),
    Index(Box<Enriched<Self, D>>, Box<Enriched<Self, D>>),
    NewArray(Box<Type>, Box<Enriched<Self, D>>),
    Cast(Box<Type>, Box<Enriched<Self, D>>),
    NewObject(Box<Type>),
    Member(Box<Enriched<Self, D>>, Box<String>),
}

#[derive(Debug, Clone)]
pub enum DeclItem<D: Default + 'static = ()> {
    NoInit(Box<String>),
    Init(Box<String>, Box<Enriched<Expr<D>, D>>),
}

impl<D: Default + 'static> DeclItem<D> {
    pub fn get_name(&self) -> String {
        match self {
            DeclItem::NoInit(name) => *name.clone(),
            DeclItem::Init(name, _) => *name.clone(),
        }
    }

    #[allow(dead_code)]
    pub fn get_val(&self) -> Option<&Enriched<Expr<D>, D>> {
        match self {
            DeclItem::NoInit(_) => None,
            DeclItem::Init(_, val) => Some(&**val),
        }
    }
}

pub type Block<D = ()> = PtrList<Enriched<Stmt<D>, D>>;

#[derive(Debug, Clone)]
pub enum Stmt<D: Default + 'static = ()> {
    Empty,
    Block(Box<Block<D>>),
    Decl(Box<Type>, Box<PtrList<Enriched<DeclItem<D>, D>>>),
    Expr(Box<Enriched<Expr<D>, D>>),
    Assign(Box<Enriched<Expr<D>, D>>, Box<Enriched<Expr<D>, D>>),
    Incr(Box<Enriched<Expr<D>, D>>),
    Decr(Box<Enriched<Expr<D>, D>>),
    ReturnVoid,
    Return(Box<Enriched<Expr<D>, D>>),
    Cond(Box<Enriched<Expr<D>, D>>, Box<Enriched<Stmt<D>, D>>),
    CondElse(
        Box<Enriched<Expr<D>, D>>,
        Box<Enriched<Stmt<D>, D>>,
        Box<Enriched<Stmt<D>, D>>,
    ), // cond, if, else
    While(Box<Enriched<Expr<D>, D>>, Box<Enriched<Stmt<D>, D>>),
    For(
        Box<Type>,
        Box<String>,
        Box<Enriched<Expr<D>, D>>,
        Box<Enriched<Stmt<D>, D>>,
    ),
}

pub type Arg = (Box<Type>, Box<String>);

pub type Program<D = ()> = Box<PtrList<Enriched<TopDef<D>, D>>>;

peg::parser!( grammar parser() for str {
    use peg::ParseLiteral;

    rule _ = [' ' | '\n']*

    rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** (_ "," _)) {v}
    rule bracketed<T>(x: rule<T>) -> T = "[" _ v:x() _ "]" {v}

    rule string_lit() -> String = "\"" s:$([^ '\"']*) "\"" {s.into()}
    rule ident() -> String = "Ident" _ s:string_lit() {s}

    rule number() -> IntValue
        = n:$(['0'..='9']+) { IntValue::Raw(Box::new(n.into())) }

    rule position() -> LineCol
        = "(" _ "Just" _ "(" _ l:number() _ "," _ c:number() _ ")" _ ")" { LineCol { line: l.value(), col: c.value() } }
        / "Nothing" { LineCol { line: -1, col: -1 } }

    pub rule program<D: Default + 'static>() -> Program<D>
        = _ "Program" _ position() _ l:bracketed(<commasep(<top_def()>)>) _ {
            Box::new(l.into_iter().map(Box::new).collect())
        }

    pub rule top_def<D: Default + 'static>() -> Enriched<TopDef<D>, D>
        = "TopClassDef" _ pos:position() _ "(" _ id:ident() _ ")" _  members:bracketed(<commasep(<class_member()>)>) {
            (TopDef::<D>::Class(Class::<D> {
                name: Box::new(id),
                super_cls: None,
                members: Box::new(members.into_iter().map(Box::new).collect()),
            }), pos, D::default())
        }
        / "TopClassDefWithInher" _ pos:position() _ "(" _ id:ident() _ ")" _  "(" _ super_cls:ident() _ ")" _ members:bracketed(<commasep(<class_member()>)>) {
            (TopDef::<D>::Class(Class {
                name: Box::new(id),
                super_cls: Some(Box::new(super_cls)),
                members: Box::new(members.into_iter().map(Box::new).collect()),
            }), pos, D::default())
        }
        / "TopFnDef" _ position() _ "(" _ fun:fun_def() _ ")" {
            (TopDef::Fun(fun.0), fun.1, D::default())
        }

    pub rule fun_def<D: Default + 'static>() -> Enriched<Routine<D>, D>
        = "FnDef"  _ pos:position() _ "(" _ t:typ() _ ")" _ "(" _ id:ident() _ ")" _ args:bracketed(<commasep(<arg()>)>) _ "(" _ b:block() _ ")" {
            (Routine::<D> {
                name: Box::new(id),
                return_type: Box::new(t),
                params: Box::new(args.into_iter().map(Box::new).collect()),
                body: Box::new(b)
            }, pos, D::default())
        }

    pub rule class_member<D: Default + 'static>() -> Enriched<ClassMember<D>, D>
       = "Method" _ position() _ "(" _ fun:fun_def() _ ")" {
            (ClassMember::Method(fun.0), fun.1, D::default())
        }
        / "Field" _ pos:position() _ "(" _ t:typ() _ ")" _ "(" _ name:ident() _ ")" {
            (ClassMember::Field {
                typ: Box::new(t),
                name: Box::new(name)
            }, pos, D::default())
        }

    pub rule arg<D: Default + 'static>() -> Enriched<Arg, D>
        = "Arg" _ pos:position() _ "(" _ t:typ() _ ")" _ "(" _ id:ident() _ ")" {
            ((Box::new(t), Box::new(id)), pos, D::default())
        }

    pub rule stmt<D: Default + 'static>() -> Enriched<Stmt<D>, D>
        = "Empty" _ pos:position() {
            (Stmt::Empty, pos, D::default())
        }
        / "Ass" _ pos:position() _ "(" _ lval:expr() _ ")" _ "(" _ rval:expr() _ ")" {
            (Stmt::Assign(Box::new(lval), Box::new(rval)), pos, D::default())
        }
        / "SExp" _ pos:position() _ "(" _ e:expr() _ ")" {
            (Stmt::Expr(Box::new(e)), pos, D::default())
        }
        / "Incr" _ pos:position() _ "(" _ e:expr() _ ")" {
            (Stmt::Incr(Box::new(e)), pos, D::default())
        }
        / "Decr" _ pos:position() _ "(" _ e:expr() _ ")" {
            (Stmt::Decr(Box::new(e)), pos, D::default())
        }
        / "Ret" _ pos:position() _ "(" _ e:expr() _ ")" {
            (Stmt::Return(Box::new(e)), pos, D::default())
        }
        / "VRet" _ pos:position() {
            (Stmt::ReturnVoid, pos, D::default())
        }
        / "BStmt" _ pos:position() _ "(" _ b:block() _ ")" {
            (Stmt::Block(Box::new(b)), pos, D::default())
        }
        / "Decl" _ pos:position() _ "(" _ t:typ() _ ")" _ items:bracketed(<commasep(<decl_item()>)>) {
            (Stmt::Decl(Box::new(t), Box::new(items.into_iter().map(Box::new).collect())), pos, D::default())
        }
        / "Cond" _ pos:position() _ "(" _ e:expr() _ ")" _ "(" _ s:stmt() _ ")" {
            (Stmt::Cond(Box::new(e), Box::new(s)), pos, D::default())
        }
        / "CondElse" _ pos:position() _ "(" _ e:expr() _ ")" _ "(" _ s1:stmt() _ ")" _ "(" _ s2:stmt() _ ")" {
            (Stmt::CondElse(Box::new(e), Box::new(s1), Box::new(s2)), pos, D::default())
        }
        / "While" _ pos:position() _ "(" _ e:expr() _ ")" _ "(" _ s:stmt() _ ")" {
            (Stmt::While(Box::new(e), Box::new(s)), pos, D::default())
        }
        / "For" _ pos:position() _ "(" _ t:typ() _ ")" _ "(" _ id:ident() _ ")" _ "(" _ e:expr() _ ")" _ "(" _ s:stmt() _ ")" {
            (Stmt::For(Box::new(t), Box::new(id), Box::new(e), Box::new(s)), pos, D::default())
        }

    pub rule block<D: Default + 'static>() -> Block<D>
        = "Block" _ position() _ l:bracketed(<commasep(<stmt()>)>) {
            l.into_iter().map(Box::new).collect()
        }

    pub rule decl_item<D: Default + 'static>() -> Enriched<DeclItem<D>, D>
        = "NoInit" _ pos:position() _ "(" _ id:ident() _ ")" {
            (DeclItem::NoInit(Box::new(id)), pos, D::default())
        }
        / "Init" _ pos:position() _ "(" _ id:ident() _ ")" _ "(" _ e:expr() _ ")"{
            (DeclItem::Init(Box::new(id), Box::new(e)), pos, D::default())
        }

    pub rule operator() -> BinaryOp
        = "Plus" _  position() { BinaryOp::Add }
        / "Minus" _  position() { BinaryOp::Sub }
        / "Times" _  position() { BinaryOp::Mul }
        / "Div" _  position() { BinaryOp::Div }
        / "Mod" _  position() { BinaryOp::Mod }
        / "LTH" _  position() { BinaryOp::Lt }
        / "LE" _  position() { BinaryOp::Lte }
        / "GTH" _  position() { BinaryOp::Gt }
        / "GE" _  position() { BinaryOp::Gte }
        / "EQU" _  position() { BinaryOp::Eq }
        / "NE" _  position() { BinaryOp::Neq }

    pub rule unary_op<D: Default + 'static>(name: &'static str, op: &UnaryOp) -> Enriched<Expr<D>, D>
        = ##parse_string_literal(name) _ pos:position() _ "(" _ e:expr() _ ")" {
            (Expr::Unary(op.clone(), Box::new(e)), pos, D::default())
        }

    pub rule binary_op<D: Default + 'static>(name: &'static str) -> Enriched<Expr<D>, D>
        = ##parse_string_literal(name) _ pos:position() _ "(" _ lhs:expr() _ ")" _ "(" _ op:operator()  _ ")" _ "(" _ rhs:expr() _ ")" {
            (Expr::Binary(op.clone(), Box::new(lhs), Box::new(rhs)), pos, D::default())
        }

    pub rule expr<D: Default + 'static>() -> Enriched<Expr<D>, D>
        = "ELitInt" _ pos:position() _ n:number() {
            let x: Expr<D> = Expr::Literal(Box::new(Value::Num(n)));
            (x, pos, D::default())
        }
        / "ELitNull" _ pos:position() {
            (Expr::Literal(Box::new(Value::Null)), pos, D::default())
        }
        / "ELitTrue" _ pos:position() {
            (Expr::Literal(Box::new(Value::Bool(true))), pos, D::default())
        }
        / "ELitFalse" _ pos:position() {
            (Expr::Literal(Box::new(Value::Bool(false))), pos, D::default())
        }
        / "ECast" _ pos:position() _  "(" _ t:typ() _ ")" _ "(" _ exp:expr() _ ")" {
            (Expr::Cast(Box::new(t), Box::new(exp)), pos, D::default())
        }
        / "EVar" _ pos:position() _ "(" _ id:ident() _ ")" {
            (Expr::Ident(Box::new(id)), pos, D::default())
        }
        / "EString" _ pos:position() _  s:string_lit() {
            (Expr::Literal(Box::new(Value::String(Box::new(s)))), pos, D::default())
        }
        / "EApp" _ pos:position() _  "(" _ lhs:expr() _ ")" _ args:bracketed(<commasep(<expr()>)>) {
            (Expr::Apply(Box::new(lhs), Box::new(args.into_iter().map(Box::new).collect())), pos, D::default())
        }
        / "EIndex" _ pos:position() _  "(" _ lval:expr() _ ")" _ "(" _ rval:expr() _ ")" {
            (Expr::Index(Box::new(lval), Box::new(rval)), pos, D::default())
        }
        / "ENewArray" _ pos:position() _  "(" _ t:typ() _ ")" _ "(" _ exp:expr() _ ")" {
            (Expr::NewArray(Box::new(t), Box::new(exp)), pos, D::default())
        }
        / "ENewObject" _ pos:position() _  "(" _ t:typ() _ ")" {
            (Expr::NewObject(Box::new(t)), pos, D::default())
        }
        / "EMember" _ pos:position() _  "(" _ exp:expr() _ ")" _ "(" _ id:ident() _ ")" {
            (Expr::Member(Box::new(exp), Box::new(id)), pos, D::default())
        }
        / "EAnd" _ pos:position() _ "(" _ lhs:expr() _ ")" _ "(" _ rhs:expr() _ ")" {
            (Expr::Binary(BinaryOp::And, Box::new(lhs), Box::new(rhs)), pos, D::default())
        }
        / "EOr" _ pos:position() _ "(" _ lhs:expr() _ ")" _ "(" _ rhs:expr() _ ")" {
            (Expr::Binary(BinaryOp::Or, Box::new(lhs), Box::new(rhs)), pos, D::default())
        }
        / op:binary_op("EAdd") {op}
        / op:binary_op("EMul") {op}
        / op:binary_op("ERel") {op}
        / op:unary_op("Neg", &UnaryOp::Neg) {op}
        / op:unary_op("Not", &UnaryOp::Not) {op}

    pub rule typ() -> Type
        = "Int" _ position() { Type::Int }
        / "Str" _ position() { Type::Str }
        / "Bool" _ position() { Type::Bool }
        / "Void" _ position() { Type::Void }
        / "Array" _ position() _ "(" _ t:typ() _ ")" { Type::Array(Box::new(t)) }
        / "Class" _ position() _ "(" _ id:ident() _ ")"  { Type::Class { name: Box::new(id), info: None } }
});

pub fn parse<D: Default + 'static>(source: &str) -> Result<Program<D>> {
    match parser::program(source) {
        Ok(program) => Ok(program),
        Err(err) => bail!(err),
    }
}

use crate::common::LineCol;
use crate::error::ErrorWithPosition;
use crate::parser::Type;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypecheckerError {
    #[error("Function: \"main\" was not found")]
    MainNotFound,
    #[error("Cyclic inheritance: {}", display_path_vect(.0, " -> "))]
    CyclicInheritance(Vec<String>),
    #[error("Keyword: {1} is reserved")]
    ReservedKeyword(Option<LineCol>, String),
    #[error("Function: {1} cannot be redeclared")]
    CannotRedeclareFunction(Option<LineCol>, String),
    #[error("Cannot use in expression: {1}")]
    CannotUseInExpression(Option<LineCol>, String),
    #[error("Undeclared identifier: {1}")]
    UndeclaredIdent(Option<LineCol>, String),
    #[error("Undeclared class: {1}")]
    UndeclaredClass(Option<LineCol>, String),
    #[error("Only class can be instantiated, got: {1}")]
    NotAClass(Option<LineCol>, Type),
    #[allow(dead_code)]
    #[error("Class {1} is not a subclass of {2}")]
    NotASubclass(Option<LineCol>, Type, Type),
    #[error("Classes {1} and {2} are not related by class hierarchy")]
    IncompatibleClasses(Option<LineCol>, String, String),
    #[error("Object of type: {1} has no member: {2}")]
    NotAMember(Option<LineCol>, Type, String),
    #[error("Type: {1} is not indexable")]
    NotIndexable(Option<LineCol>, Type),
    #[error("Type: {1} is not callable")]
    NotCallable(Option<LineCol>, Type),
    #[error("Type: {1} is not assignable to type: {2}")]
    NotAssignable(Option<LineCol>, Type, Type),
    #[error("Call argument mismatch, got: {got}, expected: {expected}", got=display_type_vect(.1, ", "), expected=display_type_vect(.2, ", "))]
    CallArgsMismatch(Option<LineCol>, Vec<Type>, Vec<Type>),
    #[error("Invalid type, got: {1}, expected: {expected}", expected=display_type_vect(.2, " or "))]
    InvalidType(Option<LineCol>, Type, Vec<Type>),
    #[error("Cannot cast from: {1} to: {2}")]
    CannotCast(Option<LineCol>, Type, Type),
    #[error("Invalid binary operator input types, got: {got}, expected: {expected}", got=display_tuple(.1), expected=display_type_tuple_vect(.2))]
    InvalidBinOpType(Option<LineCol>, (Type, Type), Vec<(Type, Type)>),
    #[error("Void type used at invalid position")]
    InvalidVoidUsage(Option<LineCol>),
    #[error("Assignment operations require l-value")]
    NotAnLValue(Option<LineCol>),
    #[error("Missing return statement in function: {1}")]
    MissingReturn(Option<LineCol>, String),
    #[error("Cannot determine if function: {1} always returns. Consider adding return statement to all conditional blocks.")]
    PotentiallyMissingReturn(Option<LineCol>, String),
    #[error("Function parameter with name: {1} is already declared")]
    RepeatedParamName(Option<LineCol>, String),
    #[error("Literal: {1} is out of range for i64 type")]
    IntOutOfRange(Option<LineCol>, String),
    #[error("Class member with name: {1} is already declared")]
    RepeatedMemberName(Option<LineCol>, String),
    #[error("Evaluation of this expression will fail at runtime")]
    EvaluationWillFailAtRuntime(Option<LineCol>),
    #[error("Overloading not supported. Trying to overload function: {fun_name} first defined in class {superclass_name} ({super_fun_type}) with new definition in class {class_name} ({fun_type})")]
    OverloadingNotSupported {
        pos: Option<LineCol>,
        class_name: String,
        superclass_name: String,
        fun_name: String,
        fun_type: Type,
        super_fun_type: Type,
    },
    #[error("Conflicting return types, got: {typ} and {prev_typ}")]
    ConflictingReturnType {
        pos: Option<LineCol>,
        typ: Type,
        prev_pos: Option<LineCol>,
        prev_typ: Type,
    },
    #[error("Name: {name} is already declared")]
    RepeatedDeclaration {
        pos: Option<LineCol>,
        prev_pos: Option<LineCol>,
        name: String,
    },
}

impl ErrorWithPosition for TypecheckerError {
    fn get_position(&self) -> Option<LineCol> {
        match self {
            Self::MainNotFound => None,
            Self::CyclicInheritance(_) => None,
            Self::ReservedKeyword(pos, ..) => pos.clone(),
            Self::CannotRedeclareFunction(pos, ..) => pos.clone(),
            Self::CannotUseInExpression(pos, ..) => pos.clone(),
            Self::UndeclaredIdent(pos, _) => pos.clone(),
            Self::UndeclaredClass(pos, _) => pos.clone(),
            Self::NotAClass(pos, _) => pos.clone(),
            Self::NotASubclass(pos, ..) => pos.clone(),
            Self::IncompatibleClasses(pos, ..) => pos.clone(),
            Self::NotAMember(pos, ..) => pos.clone(),
            Self::NotIndexable(pos, _) => pos.clone(),
            Self::NotAssignable(pos, ..) => pos.clone(),
            Self::NotCallable(pos, _) => pos.clone(),
            Self::CallArgsMismatch(pos, ..) => pos.clone(),
            Self::NotAnLValue(pos) => pos.clone(),
            Self::CannotCast(pos, ..) => pos.clone(),
            Self::InvalidType(pos, ..) => pos.clone(),
            Self::InvalidBinOpType(pos, ..) => pos.clone(),
            Self::InvalidVoidUsage(pos) => pos.clone(),
            Self::ConflictingReturnType { pos, .. } => pos.clone(),
            Self::MissingReturn(pos, ..) => pos.clone(),
            Self::PotentiallyMissingReturn(pos, ..) => pos.clone(),
            Self::RepeatedParamName(pos, ..) => pos.clone(),
            Self::RepeatedMemberName(pos, ..) => pos.clone(),
            Self::IntOutOfRange(pos, ..) => pos.clone(),
            Self::EvaluationWillFailAtRuntime(pos) => pos.clone(),
            Self::RepeatedDeclaration { pos, .. } => pos.clone(),
            Self::OverloadingNotSupported { pos, .. } => pos.clone(),
        }
    }

    fn get_prev_position(&self) -> Option<LineCol> {
        match self {
            Self::ConflictingReturnType { prev_pos, .. } => prev_pos.clone(),
            Self::RepeatedDeclaration { prev_pos, .. } => prev_pos.clone(),
            _ => None,
        }
    }
}

fn display_tuple(types: &(Type, Type)) -> String {
    format!("({}, {})", types.0.to_string(), types.1.to_string())
}

fn display_type_vect(types: &Vec<Type>, sep: &str) -> String {
    if types.len() == 0 {
        return "<NONE>".into();
    }
    types
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}

fn display_path_vect(types: &Vec<String>, sep: &str) -> String {
    if types.len() == 0 {
        return "<NONE>".into();
    }
    types
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(sep)
}

fn display_type_tuple_vect(types: &Vec<(Type, Type)>) -> String {
    if types.len() == 0 {
        return "<NONE>".into();
    }
    types
        .iter()
        .map(|t| display_tuple(t))
        .collect::<Vec<_>>()
        .join(" or ")
}

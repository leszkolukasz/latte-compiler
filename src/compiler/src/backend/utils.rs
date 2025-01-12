use crate::common::{Enriched, LineCol};
use crate::{Ext, NodeData};
use crate::parser::Type;

pub fn to_dummy_enriched<T>(ast: T, typ: Option<Type>) -> Enriched<T, Ext> {
    (ast, LineCol { line: 0, col: 0 }, NodeData { expr_typ: typ })
}

#[allow(dead_code)]
pub fn is_power_of_two(n: i64) -> bool {
    n > 0 && (n & (n - 1)) == 0
}

// Assumes: n > 0
pub fn highest_power_of_two_divisor(n: i64) -> i64 {
    i64::pow(2, n.trailing_zeros())
}
use std::fmt::Debug;

#[derive(Hash, Clone, PartialEq, Eq)]
pub struct LineCol {
    pub line: i64,
    pub col: i64,
}

impl Debug for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.col)
    }
}

// Node type, position, additional data
pub type Enriched<T, D = ()> = (T, LineCol, D);

pub type PtrList<T> = Vec<Box<T>>;

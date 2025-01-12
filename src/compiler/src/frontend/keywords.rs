use crate::common::LineCol;
use crate::frontend::error::TypecheckerError;
use crate::frontend::Context;
use anyhow::{bail, Result};

pub fn check(context: &Context, name: &str, pos: &LineCol) -> Result<()> {
    if context.reserved_keywords.contains(&name) {
        bail!(TypecheckerError::ReservedKeyword(
            Some(pos.clone()),
            String::from(name).clone(),
        ))
    }

    Ok(())
}

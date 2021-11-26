use std::{error::Error, fmt};

use crate::chronos::Position;

#[derive(Debug)]
pub struct IllegalCharError(pub Position, pub String);

impl fmt::Display for IllegalCharError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Illegal Character: {}\n File: {}, Line: {}",
            self.1, self.0.file_name, self.0.line
        )
    }
}

impl Error for IllegalCharError {}

#[derive(Debug)]
pub struct InvalidSyntaxError(pub Position, pub String);

impl fmt::Display for InvalidSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Invalid Syntax: {}\n File: {}, Line: {}",
            self.1, self.0.file_name, self.0.line
        )
    }
}

impl Error for InvalidSyntaxError {}


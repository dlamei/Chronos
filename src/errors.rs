use std::fmt;

use crate::chronos::Position;

#[derive(Debug)]
pub struct ErrDesc {
    message: String,
    details: String,
}

#[derive(Debug)]
pub enum ErrTypes {
    IllegalCharError,
    InvalidSyntaxError,
}

pub struct Error(pub ErrTypes, pub ErrDesc);

impl Error {
    pub fn new(
        error_type: ErrTypes,
        start_pos: &Position,
        _end_pos: &Position,
        details: String,
    ) -> Self {
        let message = format!("File: {}, Line: {}", start_pos.file_name, start_pos.line);
        Error(error_type, ErrDesc { message, details })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {}\n{}", self.0, self.1.details, self.1.message,)
    }
}

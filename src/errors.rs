use std::fmt;
use std::fmt::Write;

use crate::chronos::Position;

#[derive(Debug)]
pub struct ErrDesc {
    message: String,
    details: String,
}

#[derive(Debug)]
pub enum ErrType {
    IllegalCharError,
    InvalidSyntaxError,
    RuntimeError,
}

pub struct Error(pub ErrType, pub ErrDesc);

impl Error {
    pub fn new(
        error_type: ErrType,
        start_pos: &Position,
        end_pos: &Position,
        details: String,
    ) -> Self {
        let mut message = get_error_preview(&start_pos.text, start_pos, end_pos);
        write!(
            message,
            "\nFile: {}, Line: {}",
            start_pos.file_name, start_pos.line
        )
        .unwrap();

        Error(error_type, ErrDesc { message, details })
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {}\n{}", self.0, self.1.details, self.1.message)
    }
}

pub fn get_error_preview(text: &String, pos_start: &Position, pos_end: &Position) -> String {
    let mut result = String::from("");

    let mut start = text[0..pos_start.index].rfind('\n').unwrap_or(0);
    let mut end = text[0..start + 1].find('\n').unwrap_or(text.len());

    let count = pos_end.line - pos_start.line + 1;

    for i in 0..count {
        let line = &text[start..end];
        let col_start = if i == 0 { pos_start.column } else { 0 };
        let col_end = if i == count - 1 {
            pos_end.column
        } else {
            line.len() - 1
        };

        write!(
            result,
            "{}\n{}{}",
            line.replace("\n", ""),
            " ".repeat(col_start),
            "~".repeat(col_end - col_start)
        )
        .unwrap();

        start = end;
        end = text[0..start].find('\n').unwrap_or(text.len());
    }

    return result.replace("\t", "");
}

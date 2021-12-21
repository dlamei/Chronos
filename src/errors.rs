use std::{cell::RefCell, fmt, fmt::Write, rc::Rc};

use crate::chronos::{Position, Scope};

//#[derive(Debug)]
//pub struct ErrDesc {
//    message: String,
//    details: String,
//}

#[derive(Debug)]
pub enum ErrType {
    IllegalChar,
    ExpectedChar,
    InvalidSyntax,
    Runtime,
    UndefinedOperator,
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrType,
    start_pos: Position,
    end_pos: Position,
    details: String,
    scope: Option<Rc<RefCell<Scope>>>,
}

impl Error {
    pub fn new(
        error_type: ErrType,
        start_pos: &Position,
        end_pos: &Position,
        details: String,
        scope: Option<Rc<RefCell<Scope>>>,
    ) -> Self {
        Error {
            error_type,
            start_pos: start_pos.clone(),
            end_pos: end_pos.clone(),
            details,
            scope,
        }
    }

    fn generate_message(&self) -> String {
        let mut message = get_traceback(&self.scope, &self.start_pos);
        write!(message, "{:?}: {}", self.error_type, self.details,).unwrap();

        write!(
            message,
            "\n\n{}",
            get_error_preview(&self.start_pos.text, &self.start_pos, &self.end_pos)
        )
        .unwrap();

        write!(
            message,
            "\nFile: {}, Line: {}",
            self.start_pos.file_name, self.start_pos.line
        )
        .unwrap();
        message
    }

    pub fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        self.scope = Some(scope);
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.generate_message(),)
    }
}

fn get_traceback(scope: &Option<Rc<RefCell<Scope>>>, pos_start: &Position) -> String {
    let mut result = String::from("");

    if let Some(scope) = scope {
        let mut trace: Vec<String> = Vec::new();

        write!(result, "\nTraceback (most recent call last):").unwrap();
        let mut cntx = scope.clone();
        let mut pos = pos_start.clone();

        loop {
            let mut s = String::from("");

            write!(
                s,
                "\n  File: {}, Line: {}, in {}",
                pos.file_name,
                pos.line,
                cntx.borrow().display_name
            )
            .unwrap();

            trace.push(s);

            let parent: Rc<RefCell<Scope>>;

            if let Some(p) = cntx.borrow().parent.clone() {
                pos = cntx.borrow().position.clone().unwrap_or_default();
                parent = p;
            } else {
                break;
            }

            cntx = parent;
        }

        for t in trace.iter().rev() {
            write!(result, "{}", t).unwrap();
        }
    }

    writeln!(result).unwrap();
    result
}

fn get_error_preview(text: &str, pos_start: &Position, pos_end: &Position) -> String {
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

    result.replace("\t", "")
}

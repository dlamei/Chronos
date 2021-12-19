use std::cell::RefCell;
use std::fmt;
use std::fmt::Write;
use std::rc::Rc;

use crate::chronos::Context;
use crate::chronos::Position;

//#[derive(Debug)]
//pub struct ErrDesc {
//    message: String,
//    details: String,
//}

#[derive(Debug)]
pub enum ErrType {
    IllegalCharError,
    ExpectedCharError,
    InvalidSyntaxError,
    RuntimeError,
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrType,
    start_pos: Position,
    end_pos: Position,
    details: String,
    context: Option<Rc<RefCell<Context>>>,
}

impl Error {
    pub fn new(
        error_type: ErrType,
        start_pos: &Position,
        end_pos: &Position,
        details: String,
        context: Option<Rc<RefCell<Context>>>,
    ) -> Self {
        Error { error_type, start_pos: start_pos.clone(), end_pos: end_pos.clone(), details, context }
    }

    fn generate_message(&self) -> String {
       let mut message = get_traceback(&self.context, &self.start_pos);
        write!(
            message,
            "{}",
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

    pub fn set_context(&mut self, context: Rc<RefCell<Context>>) {
        self.context = Some(context);
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {}\n{}", self.error_type, self.details, self.generate_message())
    }
}

fn get_traceback(context: &Option<Rc<RefCell<Context>>>, pos_start: &Position) -> String {
    let mut result = String::from("");

    if let Some(context) = context {
        write!(result, "\nTraceback (most recent call last):\n").unwrap();
        let mut cntx = context.clone();
        let mut pos = pos_start.clone();

        loop {
            write!(
                result,
                "File: {}, Line: {}, in: {}\n",
                pos.file_name,
                pos.line,
                cntx.borrow().display_name
            )
            .unwrap();

            let parent: Rc<RefCell<Context>>;

            if let Some(p) = cntx.borrow().parent.clone() {
                pos = p.1.clone();
                parent = p.0;
            } else {
                break;
            }

            cntx = parent;
        }

    }

    write!(result, "{}", "\n").unwrap();
    result
}

fn get_error_preview(text: &String, pos_start: &Position, pos_end: &Position) -> String {
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

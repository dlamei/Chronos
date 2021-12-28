use std::{cell::RefCell, fmt::Write, rc::Rc};

use crate::chronos::{Position, Scope, File};

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

pub struct Error {
    error_type: ErrType,
    start_pos: Option<Position>,
    end_pos: Option<Position>,
    details: String,
    scope: Option<Rc<RefCell<Scope>>>,
    files: Option<Vec<File>>,
}

impl Error {
    pub fn new(
        error_type: ErrType,
        start_pos: Option<Position>,
        end_pos: Option<Position>,
        details: String,
        scope: Option<Rc<RefCell<Scope>>>,
    ) -> Self {
        Error {
            error_type,
            start_pos,
            end_pos,
            details,
            scope,
            files: None,
        }
    }

    pub fn print(&self) {
        if self.files.is_some() && self.start_pos.is_some() && self.end_pos.is_some() {
            println!("{}", self.generate_message(self.files.as_ref().unwrap()));
        } else {
            println!("{:?}: {}", self.error_type, self.details);
        }
    }

    pub fn set_files(&mut self, files: Vec<File>) {
        self.files = Some(files);
    }
    
    fn generate_message(&self, files: &[File]) -> String {
        let mut message = get_traceback(&self.scope, &self.start_pos.unwrap(), files);
        let file_name = &files.get(self.start_pos.unwrap().file_nr).unwrap().name;

        write!(message, "{:?}: {}", self.error_type, self.details,).unwrap();

        write!(
            message,
            "\n\n{}",
            get_error_preview(self.start_pos.unwrap().file_nr, files, &self.start_pos.unwrap(), &self.end_pos.unwrap())
        )
        .unwrap();

        write!(
            message,
            "\nFile: {}, Line: {}",
            file_name, self.start_pos.unwrap().line
        )
        .unwrap();
        message
    }

    pub fn set_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        self.scope = Some(scope);
    }
}

//impl fmt::Display for Error {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        write!(f, "{}", self.generate_message(),)
//    }
//}

fn get_traceback(scope: &Option<Rc<RefCell<Scope>>>, pos_start: &Position, files: &[File]) -> String {
    let mut result = String::from("");

    if let Some(scope) = scope {
        let mut trace: Vec<String> = Vec::new();

        write!(result, "\nTraceback (most recent call last):").unwrap();
        let mut cntx = scope.clone();
        let mut pos = *pos_start;

        loop {
            let mut s = String::from("");

            write!(
                s,
                "\n  File: {}, Line: {}, in {}",
                //pos.file_name,
                files.get(pos.file_nr).unwrap().name, 
                pos.line,
                cntx.borrow().display_name
            )
            .unwrap();

            trace.push(s);

            let parent: Rc<RefCell<Scope>>;

            if let Some(p) = cntx.borrow().parent.clone() {
                pos = cntx.borrow().position.unwrap_or_default();
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

fn get_error_preview(file_nr: usize, files: &[File], pos_start: &Position, pos_end: &Position) -> String {
    let mut result = String::from("");
    let file = files.get(file_nr).unwrap();
    let text = &file.text;

    let mut start = text[0..pos_start.offset + pos_start.index].rfind('\n').unwrap_or(0);
    let mut end = text[start + 1..].find('\n').unwrap_or(text.len() - start + 1) + start + 1;

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
        end = text[0..start].find('\n').unwrap_or_else(|| text.len());
    }

    result.replace("\t", "")
}

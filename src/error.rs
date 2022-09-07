use crate::lexer::Position;
use colored::Colorize;

pub fn underline_code(code: &str, pos: &Position) -> String {
    let mut beg_line = code[0..pos.start].rfind('\n').unwrap_or(0);
    if beg_line != 0 {
        beg_line += 1
    };

    let end_line = code[pos.start..]
        .find('\n')
        .unwrap_or(code.len() - pos.start)
        + pos.start;

    let err_start = pos.start - beg_line;
    let err_len = pos.end - pos.start;

    let mut line = code[beg_line..end_line].to_string();
    line.push('\n');
    let arrows = " ".repeat(err_start) + &"⌃".repeat(err_len);

    line + &arrows.red().to_string()
}

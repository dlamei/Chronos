use colored::Colorize;
use crate::lexer::Position;

pub fn underline_code(code: &String, pos: &Position) {
    if pos.end - pos.start <= 0 {
        return;
    }
    //TODO:: temp
    let new_lines: Vec<usize> = code
        .chars()
        .enumerate()
        .filter(|(_, x)| *x == '\n')
        .map(|(i, _)| i)
        .collect();

    let mut beg_line = 0;
    let mut end_line = 0;

    for (i, line_indx) in new_lines.iter().enumerate() {
        if pos.start >= *line_indx {
            beg_line = i;
        }
        if pos.end >= *line_indx {
            end_line = i + 1;
        }
    }

    let start_indx: usize = *new_lines.get(beg_line).unwrap();
    let end_indx: usize = *new_lines.get(end_line).unwrap_or(&code.len());


    let err_start = pos.start - start_indx - 1;
    let err_len = pos.end - pos.start;

    let line = code[start_indx..end_indx].to_string();
    let arrows = " ".repeat(err_start) + &"⌃".repeat(err_len);

    println!("{}", line);
    println!("{}", arrows.red());

}

use crate::lexer::Range;
use colored::Colorize;

//pub fn underline_code(code: &str, pos: &Range) -> String {
//    if pos.start >= code.len() {
//        return "".to_owned();
//    }
//
//    let mut beg_line = code[0..pos.start].rfind('\n').unwrap_or(0);
//    if beg_line != 0 {
//        beg_line += 1
//    };
//
//    let end_line = code[pos.start..]
//        .find('\n')
//        .unwrap_or(code.len() - pos.start)
//        + pos.start;
//
//    let err_start = pos.start - beg_line;
//    let err_len = pos.end - pos.start;
//
//    let mut line = code[beg_line..end_line].to_string();
//    line.push('\n');
//    let arrows = " ".repeat(err_start) + &"⌃".repeat(err_len);
//
//    let arrow_line: String = arrows.red().to_string();
//    line + &arrow_line
//}

pub fn underline_code(s: &str, range: &Range) -> String {
    let mut result = String::new();
    let mut start = 0;
    for line in s.lines() {
        let end = start + line.len();

        if range.start < end && range.end > start {
            // if the range overlaps with this line
            let mut underline = String::new();
            for i in start..end {
                // iterate over each character index
                if i >= range.start && i < range.end {
                    // if the index is within the range
                    underline.push('^'); // add an underline symbol
                } else {
                    underline.push(' '); // add a space symbol
                }
            }
            result.push_str(&format!("{}\n{}\n", line, underline.red())); // append the line and the underline to the result
        } else {
            //result.push_str(&format!("{}\n", line)); // append only the line to the result
        }
        start = end + 1; // update the start index for next line (+1 for newline character)
    }
    result
}

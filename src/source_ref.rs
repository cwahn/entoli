use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceRef {
    pub source: Rc<String>,
    pub start: u32,
    pub end: u32,
}

impl SourceRef {
    pub fn new(source: Rc<String>, start: u32, end: u32) -> Self {
        SourceRef { source, start, end }
    }

    pub fn resolve(&self) -> &str {
        let start = self.start as usize;
        let end = self.end as usize;
        &self.source[start..end]
    }

    pub fn error(&self, error_message: &str) -> String {
        let source_str = &self.source;
        let source_len = source_str.len() as u32;

        // Validate positions and clamp to valid range
        let start_pos = self.start.min(source_len);
        let end_pos = self.end.min(source_len);

        // Get line and column information
        let (start_line, start_col) = Self::get_line_and_column(&self.source, start_pos);
        let (end_line, end_col) = Self::get_line_and_column(&self.source, end_pos);

        let mut result = String::new();

        // Create the location header
        result.push_str(&format!("--> input:{}:{}\n", start_line + 1, start_col + 1));
        result.push_str(" |\n");

        // Handle single-line or multi-line scenarios
        if start_line == end_line {
            // Single line case
            let line_content = Self::get_line_content(&self.source, start_line);
            let line_number = start_line + 1;

            // Format line number
            result.push_str(&format!("{:>3} | {}\n", line_number, line_content));

            // Create the underline pointer
            let mut underline = String::from(" ".repeat(start_col as usize + 3 + 1)); // 3 for line number, 1 for '|'
            let pointer_length = (end_pos - start_pos) as usize;
            underline.push_str(&"^".repeat(pointer_length.max(1))); // Ensure at least one caret

            result.push_str(&format!(" {}  {}\n", "|", underline));

            // Add the error message below
            result.push_str(&format!(" {}  {}\n", "|", error_message));
        } else {
            // Multi-line case
            for line_num in start_line..=end_line {
                let line_content = Self::get_line_content(&self.source, line_num);
                let display_line_num = line_num + 1;

                // Format line number
                result.push_str(&format!("{:>3} | {}\n", display_line_num, line_content));

                // Add carets for the first and last lines
                if line_num == start_line {
                    let mut underline = String::from(" ".repeat(start_col as usize + 4 + 1));
                    let remaining_chars = line_content.len() - start_col as usize;
                    underline.push_str(&"^".repeat(remaining_chars));
                    result.push_str(&format!(" {}  {}\n", "|", underline));
                } else if line_num == end_line {
                    let underline =
                        String::from(" ".repeat(4 + 1)) + &"^".repeat(end_col as usize + 1);
                    result.push_str(&format!(" {}  {}\n", "|", underline));
                }
            }

            // Add the error message below
            result.push_str(&format!(" {}  {}\n", "|", error_message));
        }

        result
    }

    // Helper method to get line and column from position
    fn get_line_and_column(source: &String, pos: u32) -> (u32, u32) {
        let pos_usize = pos as usize;

        if pos_usize >= source.len() {
            // If position is beyond the source, return the last line and column
            let line_count = source.chars().filter(|&c| c == '\n').count() as u32;
            let last_line = source.rfind('\n').unwrap_or(0);
            return (line_count, (source.len() - last_line - 1) as u32);
        }

        let text_before = &source[..pos_usize];
        let line = text_before.chars().filter(|&c| c == '\n').count() as u32;

        // Find the position of the last newline before the position
        let last_newline_pos = text_before.rfind('\n').map_or(0, |pos| pos + 1);
        let column = (pos_usize - last_newline_pos) as u32;

        (line, column)
    }

    // Helper method to get the content of a specific line
    fn get_line_content(source: &String, line_number: u32) -> String {
        let lines: Vec<&str> = source.split('\n').collect();

        if line_number as usize >= lines.len() {
            return String::new();
        }

        lines[line_number as usize].to_string()
    }
}

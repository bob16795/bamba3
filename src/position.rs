use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct FileRange {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub col: usize,
    pub file: String,
    pub pos: usize,
}

pub fn default_pos() -> FileRange {
    FileRange {
        start: Position {
            line: 0,
            col: 0,
            pos: 0,
            file: "???".to_string(),
        },
        end: Position {
            line: 0,
            col: 0,
            pos: 0,
            file: "???".to_string(),
        },
    }
}

impl Position {
    pub fn advance(&mut self, ch: char) {
        self.pos += 1;

        self.col += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        }
    }
}

impl fmt::Display for FileRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let text = &std::fs::read_to_string(self.start.file.clone())
            .unwrap()
            .split("\n")
            .collect::<Vec<_>>()[self.start.line - 1..=self.end.line - 1]
            .iter()
            .enumerate()
            .map(|(x, y)| format!("{:width$} |{}", self.start.line + x, y, width = 4))
            .collect::<Vec<_>>()
            .join("\n");
        write!(
            f,
            "--> {}<{}:{}>-<{}:{}>\n\n{}",
            self.start.file, self.start.line, self.start.col, self.end.line, self.end.col, text
        )
    }
}

impl From<std::ops::Range<Position>> for FileRange {
    fn from(other: std::ops::Range<Position>) -> Self {
        return FileRange {
            start: other.start,
            end: other.end,
        };
    }
}

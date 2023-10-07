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
}

pub fn default_pos() -> FileRange {
    FileRange {
        start: Position {
            line: 0,
            col: 0,
            file: "???".to_string(),
        },
        end: Position {
            line: 0,
            col: 0,
            file: "???".to_string(),
        },
    }
}

impl Position {
    pub fn advance(&mut self, ch: char) {
        self.col += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        }
    }
}

impl fmt::Display for FileRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "<{}:{}>-<{}:{}>@{}",
            self.start.line, self.start.col, self.end.line, self.end.col, self.start.file
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

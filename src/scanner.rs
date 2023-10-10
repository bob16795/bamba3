use crate::position::{FileRange, Position};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    // single char tokens
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,

    Underscore,
    SemiColon,
    Ampersand,
    Percent,
    Dollar,
    Tilde,
    Colon,
    Comma,
    Slash,
    Star,
    Plus,
    Dot,
    Bar,
    Lt,
    Gt,

    // multi char tokens
    Equal,
    EqualEqual,
    Exclaim,
    ExclaimEqual,
    Minus,
    Arrow,

    // keywords
    Comptime,
    Return,
    Extern,
    Import,
    Break,
    Class,
    Force,
    Embed,
    Const,
    While,
    Emit,
    Prop,
    Else,
    Def,
    New,
    For,
    If,
    Fn,
    In,

    // multi char
    Identifier,
    String,
    Number,
    Float,
    Char,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,

    pub pos: FileRange,
}

pub struct Scanner {
    src: String,
    slice: std::ops::Range<usize>,

    pub tok_start: Position,
    pub pos: Position,
    pub far: Position,
}

pub fn get_alpha_ident() -> Vec<char> {
    let mut result = ('a'..='z').collect::<Vec<_>>();
    result.extend(('A'..='Z').collect::<Vec<_>>());
    result.extend(('0'..='9').collect::<Vec<_>>());
    result.push('_');

    result
}

impl Scanner {
    pub fn get_checkpoint(&mut self) -> (std::ops::Range<usize>, Position) {
        self.skip_whitespace();

        (self.slice.clone(), self.pos.clone())
    }

    pub fn set_checkpoint(&mut self, pos: (std::ops::Range<usize>, Position)) {
        (self.slice, self.pos) = pos;
    }

    pub fn new(src: String, file: String) -> Self {
        Scanner {
            src,
            slice: 0..1,

            tok_start: Position {
                line: 1,
                col: 1,
                pos: 0,
                file: file.clone(),
            },

            pos: Position {
                line: 1,
                col: 1,
                pos: 0,
                file: file.clone(),
            },

            far: Position {
                line: 1,
                col: 1,
                pos: 0,
                file,
            },
        }
    }

    pub fn match_next(&mut self, k: TokenKind) -> Option<Token> {
        let start = (self.slice.clone(), self.pos.clone());

        let next = self.next();

        match next {
            Some(Token {
                value: _,
                kind,
                pos: _,
            }) => {
                if kind != k {
                    (self.slice, self.pos) = start;
                    None
                } else {
                    next.clone()
                }
            }
            _ => {
                (self.slice, self.pos) = start;
                None
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_at_end() {
            return None;
        }
        self.slice.end += 1;

        let ch = self.src[self.get_slice()].chars().last();

        if ch.is_some() {
            self.pos.advance(ch.unwrap());
        }

        ch
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.is_at_end() {
                return;
            }

            let c = self.src[self.slice.clone()].chars().last();
            match c {
                Some(' ' | '\r' | '\t') => _ = self.advance(),
                Some('\n') => {
                    _ = self.advance();
                }
                Some('/') => {
                    let next = self.src.chars().nth(self.slice.end);
                    if next == Some('/') {
                        while !self.is_at_end()
                            && self.src.chars().nth(self.slice.end) != Some('\n')
                        {
                            _ = self.advance()
                        }

                        self.slice.end += 1;
                    } else {
                        return;
                    }
                }
                _ => return,
            };
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.slice.end > self.src.len()
    }

    fn get_slice(&self) -> std::ops::Range<usize> {
        self.slice.start..self.slice.end - 1
    }

    fn ch(&mut self) -> Token {
        while !self.is_at_end() && self.src[self.slice.clone()].chars().last().unwrap() != '\'' {
            _ = self.advance();
        }

        self.slice.start += 1;

        let value = (self.src[self.get_slice()].chars().nth(0).unwrap() as i32).to_string();

        let kind = TokenKind::Number;

        _ = self.advance();

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }

    fn ident_string(&mut self) -> Token {
        while !self.is_at_end() && self.src[self.slice.clone()].chars().last().unwrap() != '`' {
            _ = self.advance();
        }

        self.slice.start += 1;

        let value = self.src[self.get_slice()].to_string();

        let kind = TokenKind::Identifier;

        _ = self.advance();

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }

    fn builtin(&mut self) -> Token {
        while !self.is_at_end()
            && get_alpha_ident().contains(&self.src[self.slice.clone()].chars().last().unwrap())
        {
            _ = self.advance();
        }

        let mut value = self.src[self.get_slice()].to_string();

        let kind = TokenKind::Identifier;

        value = value.replace("\\n", "\n");
        value = value.replace("\\t", "\t");
        value = value.replace("\\r", "\r");
        value = value.replace("\\0", "\x00");

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }

    fn string(&mut self) -> Token {
        while self.src[self.slice.clone()].chars().last().unwrap() != '"' {
            _ = self.advance();
        }

        self.slice.start += 1;

        let mut value = self.src[self.get_slice()].to_string();

        let kind = TokenKind::String;

        _ = self.advance();

        value = value.replace("\\n", "\n");
        value = value.replace("\\t", "\t");
        value = value.replace("\\r", "\r");
        value = value.replace("\\0", "\x00");

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }

    fn number(&mut self) -> Token {
        while !self.is_at_end()
            && ("0123456789.").contains(self.src[self.slice.clone()].chars().last().unwrap())
        {
            _ = self.advance();
        }

        let value = self.src[self.get_slice()].to_string();

        let kind = if value.contains('.') {
            TokenKind::Float
        } else {
            TokenKind::Number
        };

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }

    fn identifier(&mut self) -> Token {
        while !self.is_at_end()
            && get_alpha_ident().contains(&self.src[self.slice.clone()].chars().last().unwrap())
        {
            _ = self.advance();
        }

        let value = self.src[self.get_slice()].to_string();

        let kind = match value.as_str() {
            "comptime" => TokenKind::Comptime,
            "return" => TokenKind::Return,
            "extern" => TokenKind::Extern,
            "import" => TokenKind::Import,
            "const" => TokenKind::Const,
            "while" => TokenKind::While,
            "embed" => TokenKind::Embed,
            "class" => TokenKind::Class,
            "break" => TokenKind::Break,
            "force" => TokenKind::Force,
            "emit" => TokenKind::Emit,
            "prop" => TokenKind::Prop,
            "else" => TokenKind::Else,
            "def" => TokenKind::Def,
            "new" => TokenKind::New,
            "for" => TokenKind::For,
            "if" => TokenKind::If,
            "in" => TokenKind::In,
            "fn" => TokenKind::Fn,
            _ => TokenKind::Identifier,
        };

        Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        }
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_at_end() {
            return None;
        }

        self.skip_whitespace();

        self.slice.start = self.slice.end - 1;

        self.tok_start = self.pos.clone();

        let ch = match self.advance() {
            None => return None,
            Some(ch) => ch,
        };

        if self.far.line < self.pos.line {
            self.far = self.pos.clone();
        }
        if self.far.line == self.pos.line && self.far.col < self.pos.col {
            self.far = self.pos.clone();
        }

        let kind = match ch {
            'a'..='z' | 'A'..='Z' => return Some(self.identifier()),
            '"' => return Some(self.string()),
            '`' => return Some(self.ident_string()),
            '\'' => return Some(self.ch()),
            '@' => return Some(self.builtin()),
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Bar,
            '+' => TokenKind::Plus,
            '.' => TokenKind::Dot,
            '$' => TokenKind::Dollar,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '_' => TokenKind::Underscore,
            ';' => TokenKind::SemiColon,
            ':' => TokenKind::Colon,
            '/' => TokenKind::Slash,
            '~' => TokenKind::Tilde,
            '%' => TokenKind::Percent,
            ',' => TokenKind::Comma,
            '*' => TokenKind::Star,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,

            '!' => {
                let peek = self.src[self.slice.clone()].chars().last();
                match peek {
                    Some('=') => {
                        _ = self.advance();
                        TokenKind::ExclaimEqual
                    }
                    _ => TokenKind::Exclaim,
                }
            }
            '=' => {
                let peek = self.src[self.slice.clone()].chars().last();
                match peek {
                    Some('=') => {
                        _ = self.advance();
                        TokenKind::EqualEqual
                    }
                    _ => TokenKind::Equal,
                }
            }
            '-' => {
                let peek = self.src[self.slice.clone()].chars().last();
                match peek {
                    Some('>') => {
                        _ = self.advance();
                        TokenKind::Arrow
                    }
                    _ => TokenKind::Minus,
                }
            }
            '0'..='9' => return Some(self.number()),

            _ => {
                return None;
            }
        };

        let value = self.src[self.get_slice()].to_string();

        Some(Token {
            value,
            kind,
            pos: (self.tok_start.clone()..self.pos.clone()).into(),
        })
    }
}

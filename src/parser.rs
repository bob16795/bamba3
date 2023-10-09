use crate::nodes::*;
use crate::position::FileRange;
use crate::scanner;

pub trait Parsable {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self>
    where
        Self: Sized;
}

#[derive(Debug, Clone)]
pub struct ConstRealExpression {
    pub pos: FileRange,

    pub value: f64,
}

impl Parsable for ConstRealExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let num = scn.match_next(scanner::TokenKind::Float);
        if num == None {
            (scn.slice, scn.pos) = start;

            return None;
        }

        Some(ConstRealExpression {
            pos: (start.1..scn.pos.clone()).into(),
            value: num.unwrap().value.parse().unwrap(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ConstIntExpression {
    pub pos: FileRange,

    pub value: i128,
}

impl Parsable for ConstIntExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let num = scn.match_next(scanner::TokenKind::Number);
        if num == None {
            (scn.slice, scn.pos) = start;

            return None;
        }

        Some(ConstIntExpression {
            pos: (start.1..scn.pos.clone()).into(),
            value: num.unwrap().value.parse().unwrap(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ClassExpression {
    pub pos: FileRange,

    pub defs: Vec<Definition>,
}

impl Parsable for ClassExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Class).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::LeftBrace).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let mut body = Vec::new();

        while scn.match_next(scanner::TokenKind::RightBrace).is_none() {
            let def = Definition::parse(scn);
            if def.is_some() {
                body.push(def.unwrap());
            } else {
                if scn.match_next(scanner::TokenKind::RightBrace).is_none() {
                    (scn.slice, scn.pos) = start;
                    return None;
                }

                break;
            }
        }

        Some(ClassExpression {
            pos: (start.1..scn.pos.clone()).into(),
            defs: body,
        })
    }
}

#[derive(Debug, Clone)]
pub struct EmitExpression {
    pub pos: FileRange,

    pub expr: statement::Statement,
}

impl Parsable for EmitExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Emit).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let parsed = statement::Statement::parse(scn);

        if parsed.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        return Some(EmitExpression {
            pos: (start.1..scn.pos.clone()).into(),

            expr: parsed.unwrap(),
        });
    }
}

#[derive(Debug, Clone)]
pub struct ComptimeExpression {
    pub pos: FileRange,

    pub expr: statement::Statement,
}

impl Parsable for ComptimeExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Comptime).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let parsed = statement::Statement::parse(scn);

        if parsed.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        return Some(ComptimeExpression {
            pos: (start.1..scn.pos.clone()).into(),

            expr: parsed.unwrap(),
        });
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub pos: FileRange,

    pub name: String,
    pub value: top_expression::TopExpression,
    pub force: bool,
}

impl Parsable for Definition {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let force = scn.match_next(scanner::TokenKind::Force).is_some();

        if scn.match_next(scanner::TokenKind::Def).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let name = scn.match_next(scanner::TokenKind::Identifier);
        if name.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let name_str = name.unwrap().value;

        if scn.match_next(scanner::TokenKind::Colon).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let expr = top_expression::TopExpression::parse(scn);

        if expr.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::SemiColon).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        Some(Definition {
            pos: (start.1..scn.pos.clone()).into(),

            name: name_str,
            value: expr.unwrap(),
            force,
        })
    }
}

#[derive(Debug, Clone)]
pub struct File {
    pub pos: FileRange,

    pub defs: Vec<Definition>,
}

impl Parsable for File {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let mut nodes = Vec::new();

        loop {
            let parsed = Definition::parse(scn);

            if parsed.is_none() {
                break;
            }

            nodes.push(parsed.unwrap());
        }

        if scn.is_at_end() {
            Some(File {
                defs: nodes,

                pos: (start.1..scn.pos.clone()).into(),
            })
        } else {
            None
        }
    }
}

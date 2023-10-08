use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pos: FileRange,

    name: String,
    kind: Option<top_expression::TopExpression>,
}

impl Parsable for FunctionParam {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let name = scn.match_next(scanner::TokenKind::Identifier);
        if name.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let name_str = name.unwrap().value;

        if scn.match_next(scanner::TokenKind::Colon).is_none() {
            return Some(FunctionParam {
                pos: (start.1..scn.pos.clone()).into(),

                name: name_str,
                kind: None,
            });
        }

        let expr = top_expression::TopExpression::parse(scn);

        if expr.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        return Some(FunctionParam {
            pos: (start.1..scn.pos.clone()).into(),

            name: name_str,
            kind: Some(expr.unwrap()),
        });
    }
}

#[derive(Debug, Clone)]
pub struct FunctionHeader {
    input: Vec<FunctionParam>,
    output: top_expression::TopExpression,
}

impl Parsable for FunctionHeader {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::LeftBracket).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let mut input = Vec::new();

        while scn.match_next(scanner::TokenKind::RightBracket).is_none() {
            let def = FunctionParam::parse(scn);
            if def.is_some() {
                input.push(def.unwrap());
                if scn.match_next(scanner::TokenKind::Comma).is_none() {
                    if scn.match_next(scanner::TokenKind::RightBracket).is_none() {
                        (scn.slice, scn.pos) = start;
                        return None;
                    }

                    break;
                }
            } else {
                if scn.match_next(scanner::TokenKind::RightBracket).is_none() {
                    (scn.slice, scn.pos) = start;
                    return None;
                }

                break;
            }
        }

        if scn.match_next(scanner::TokenKind::Arrow).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let output = top_expression::TopExpression::parse(scn);

        if output.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        Some(FunctionHeader {
            input,
            output: output.unwrap(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pos: FileRange,

    ext: bool,
    header: FunctionHeader,
    body: Option<statement::Statement>,
}

impl Parsable for FunctionExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let ext = scn.match_next(scanner::TokenKind::Extern).is_some();

        if scn.match_next(scanner::TokenKind::Fn).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let header = FunctionHeader::parse(scn);

        if header.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let body = statement::Statement::parse(scn);

        Some(FunctionExpression {
            pos: (start.1..scn.pos.clone()).into(),

            ext,
            header: header.unwrap(),
            body,
        })
    }
}

impl<'a> Visitable<'a> for FunctionExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let mut kind = FunctionKind::Standard;
        if self.ext {
            kind = FunctionKind::Extern;
        }

        let mut params = Vec::new();

        for p in &self.header.input.clone() {
            let val = match &p.kind {
                Some(v) => Some({
                    let res = v.visit(ctx.clone())?;
                    res.borrow().clone().visit()?;
                    res
                }),
                None => None,
            };

            params.push(NodeFunctionParam {
                name: p.name.clone(),
                pos: p.pos.clone(),
                val,
            });
        }

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::Function {
                input: params,
                output: self.header.output.visit(ctx.clone())?,
                kind,
                conts: self.body.clone(),

                name: "Anon".to_string(),

                ctx: ctx.clone(),

                impls: Rc::new(RefCell::new(HashMap::new())),
            }),
            ctx,
        })))
    }

    fn emit(&self, _ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        Err(Error::BambaError {
            data: ErrorData::TodoError("Todo: emit function expr".to_string()),
            pos: self.pos.clone(),
        })
    }
}

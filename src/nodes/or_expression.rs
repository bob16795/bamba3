use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum OrExpressionChild {
    Or(and_expression::AndExpression, OrExpression),
    AndExpression(and_expression::AndExpression),
}

#[derive(Debug, Clone)]
pub struct OrExpression {
    pos: FileRange,

    child: Box<OrExpressionChild>,
}

impl Parsable for OrExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());
        let first = and_expression::AndExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Bar).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(OrExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(OrExpressionChild::Or(first.unwrap(), next.unwrap())),
            });
        }

        Some(OrExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(OrExpressionChild::AndExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for OrExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            OrExpressionChild::Or(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a | b)),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "or".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            OrExpressionChild::AndExpression(and) => and.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            OrExpressionChild::Or(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a | b))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "or".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            OrExpressionChild::AndExpression(and) => and.emit(ctx),
        }
    }
}

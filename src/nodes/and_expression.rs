use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum AndExpressionChild {
    And(compare_expression::CompareExpression, AndExpression),
    CompareExpression(compare_expression::CompareExpression),
}

#[derive(Debug, Clone)]
pub struct AndExpression {
    pos: FileRange,

    child: Box<AndExpressionChild>,
}

impl Parsable for AndExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());
        let first = compare_expression::CompareExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Ampersand).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(AndExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(AndExpressionChild::And(first.unwrap(), next.unwrap())),
            });
        }

        Some(AndExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(AndExpressionChild::CompareExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for AndExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            AndExpressionChild::And(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a & b)),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "and".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            AndExpressionChild::CompareExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            AndExpressionChild::And(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a & b))),
                    (
                        Value::Value {
                            val: av,
                            kind: a_type,
                        },
                        Value::Value {
                            val: bv,
                            kind: b_type,
                        },
                    ) => {
                        let a_type = a_type.try_into()?;
                        let b_type = b_type.try_into()?;

                        match (&a_type, &b_type) {
                            (
                                Value::IntType {
                                    size: asize,
                                    signed: asign,
                                },
                                Value::IntType {
                                    size: bsize,
                                    signed: bsign,
                                },
                            ) => {
                                if asize != bsize || asign != bsign {
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "and".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_and(
                                    av.into_int_value(),
                                    bv.into_int_value(),
                                    "bitand",
                                );

                                Ok(Some(Value::Value {
                                    val: Rc::new(result.unwrap().into()),
                                    kind: Rc::new(RefCell::new(Node {
                                        pos: self.pos.clone(),
                                        ctx: ctx.clone(),

                                        value: NodeV::Visited(a_type.clone()),
                                    })),
                                }))
                            }

                            _ => Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "and".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }
                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "and".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            AndExpressionChild::CompareExpression(compare) => compare.emit(ctx),
        }
    }
}

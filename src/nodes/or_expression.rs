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
        let start = scn.get_checkpoint();
        let first = and_expression::AndExpression::parse(scn);

        if first.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::Bar).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                scn.set_checkpoint(start);
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
                    (Value::ConstBool(a), Value::ConstBool(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstBool(a || b)),
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
                    (
                        Value::Value {
                            val: av,
                            kind: a_type,
                            ..
                        },
                        Value::Value {
                            val: bv,
                            kind: b_type,
                            ..
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
                                            kind: "or".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_or(
                                    av.into_int_value(),
                                    bv.into_int_value(),
                                    "bitor",
                                );

                                Ok(Some(Value::Value {
                                    val: Rc::new(result.unwrap().into()),
                                    kind: Rc::new(RefCell::new(Node {
                                        pos: self.pos.clone(),
                                        ctx: ctx.clone(),

                                        value: NodeV::Visited(a_type.clone()),
                                    })),
                                    dropable: false,
                                }))
                            }

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

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        match self.child.as_ref() {
            OrExpressionChild::Or(a, b) => Ok(a.uses(name)? || b.uses(name)?),
            OrExpressionChild::AndExpression(c) => c.uses(name),
        }
    }
}

use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum TermExpressionChild {
    Add(factor_expression::FactorExpression, TermExpression),
    Sub(factor_expression::FactorExpression, TermExpression),
    FactorExpression(factor_expression::FactorExpression),
}

#[derive(Debug, Clone)]
pub struct TermExpression {
    pos: FileRange,

    child: Box<TermExpressionChild>,
}

impl Parsable for TermExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());
        let first = factor_expression::FactorExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Minus).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(TermExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(TermExpressionChild::Sub(first.unwrap(), next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Plus).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(TermExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(TermExpressionChild::Add(first.unwrap(), next.unwrap())),
            });
        }

        Some(TermExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(TermExpressionChild::FactorExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for TermExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        match self.child.as_ref() {
            TermExpressionChild::Sub(l, r) => {
                let a = l.visit(ctx.clone())?;
                let b = r.visit(ctx.clone())?;

                match (a.into(), b.into()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: self.pos.clone(),
                        value: NodeV::Visited(Value::ConstInt(a - b)),
                    }))),
                    (Value::ConstReal(a), Value::ConstReal(b)) => Ok(Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: self.pos.clone(),
                        value: NodeV::Visited(Value::ConstReal(a - b)),
                    }))),
                    (a, b) => Err(Error {
                        message: format!("Cant visit a subtract for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            TermExpressionChild::Add(l, r) => {
                let a = l.visit(ctx.clone())?;
                let b = r.visit(ctx.clone())?;

                match (a.into(), b.into()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: self.pos.clone(),
                        value: NodeV::Visited(Value::ConstInt(a + b)),
                    }))),
                    (Value::ConstReal(a), Value::ConstReal(b)) => Ok(Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: self.pos.clone(),
                        value: NodeV::Visited(Value::ConstReal(a + b)),
                    }))),
                    (a, b) => Err(Error {
                        message: format!("Cant visit a add for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            TermExpressionChild::FactorExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        match self.child.as_ref() {
            TermExpressionChild::Sub(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a - b))),
                    (Value::ConstReal(a), Value::ConstReal(b)) => Ok(Some(Value::ConstReal(a - b))),
                    (
                        Value::Value {
                            val: a,
                            kind: a_type,
                        },
                        Value::Value {
                            val: b,
                            kind: b_type,
                        },
                    ) => {
                        let a_type = a_type.into();
                        let b_type = b_type.into();

                        match (&a_type, &b_type) {
                            (Value::DoubleType, Value::DoubleType) => {
                                let br = ctx.borrow();

                                let result = br.builder.build_float_sub(
                                    a.into_float_value(),
                                    b.into_float_value(),
                                    "sub",
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
                                    return Err(Error {
                                        message: format!(
                                            "Cant emit an subtract for the types {} {}",
                                            a_type, b_type
                                        ),
                                        pos: Some(self.pos.clone()),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_sub(
                                    a.into_int_value(),
                                    b.into_int_value(),
                                    "subtract",
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

                            _ => Err(Error {
                                message: format!(
                                    "Cant emit a subtract for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            }),
                        }
                    }

                    _ => Err(Error {
                        message: format!("Cant emit a subtract for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            TermExpressionChild::Add(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a + b))),
                    (
                        Value::Value {
                            val: a,
                            kind: a_type,
                        },
                        Value::Value {
                            val: b,
                            kind: b_type,
                        },
                    ) => {
                        let a_type = a_type.into();
                        let b_type = b_type.into();

                        match (&a_type, &b_type) {
                            (Value::DoubleType, Value::DoubleType) => {
                                let br = ctx.borrow();

                                let result = br.builder.build_float_add(
                                    a.into_float_value(),
                                    b.into_float_value(),
                                    "add",
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
                                    return Err(Error {
                                        message: format!(
                                            "Cant emit an add for the types {} {}",
                                            a_type, b_type
                                        ),
                                        pos: Some(self.pos.clone()),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_add(
                                    a.into_int_value(),
                                    b.into_int_value(),
                                    "add",
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

                            _ => Err(Error {
                                message: format!(
                                    "Cant emit an add for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            }),
                        }
                    }

                    _ => Err(Error {
                        message: format!("Cant emit a subtract for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            TermExpressionChild::FactorExpression(compare) => compare.emit(ctx),
        }
    }
}
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum FactorExpressionChild {
    Mod(unary_expression::UnaryExpression, FactorExpression),
    Div(unary_expression::UnaryExpression, FactorExpression),
    Mul(unary_expression::UnaryExpression, FactorExpression),
    UnaryExpression(unary_expression::UnaryExpression),
}

#[derive(Debug, Clone)]
pub struct FactorExpression {
    pos: FileRange,

    child: Box<FactorExpressionChild>,
}

impl Parsable for FactorExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());
        let first = unary_expression::UnaryExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Percent).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(FactorExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(FactorExpressionChild::Mod(first.unwrap(), next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Slash).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(FactorExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(FactorExpressionChild::Div(first.unwrap(), next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Star).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(FactorExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(FactorExpressionChild::Mul(first.unwrap(), next.unwrap())),
            });
        }

        Some(FactorExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(FactorExpressionChild::UnaryExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for FactorExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        match self.child.as_ref() {
            FactorExpressionChild::Mul(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a * b)),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant emit a multiply for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            FactorExpressionChild::Div(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a / b)),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant emit a divide for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            FactorExpressionChild::Mod(_l, _r) => Err(Error {
                message: format!("Todo: visit mod expression"),
                pos: Some(self.pos.clone()),
            }),
            FactorExpressionChild::UnaryExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        match self.child.as_ref() {
            FactorExpressionChild::Mul(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a * b))),
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

                                let result = br.builder.build_float_mul(
                                    a.into_float_value(),
                                    b.into_float_value(),
                                    "mul",
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
                                            "Cant emit an mul for the types {} {}",
                                            a_type, b_type
                                        ),
                                        pos: Some(self.pos.clone()),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_mul(
                                    a.into_int_value(),
                                    b.into_int_value(),
                                    "mul",
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
                                    "Cant emit an mul for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            }),
                        }
                    }

                    _ => Err(Error {
                        message: format!("Cant emit a mul for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            FactorExpressionChild::Div(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a / b))),
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

                                let result = br.builder.build_float_div(
                                    a.into_float_value(),
                                    b.into_float_value(),
                                    "div",
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
                                            "Cant emit an div for the types {} {}",
                                            a_type, b_type
                                        ),
                                        pos: Some(self.pos.clone()),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_unsigned_div(
                                    a.into_int_value(),
                                    b.into_int_value(),
                                    "div",
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
                                    "Cant emit an div for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            }),
                        }
                    }

                    _ => Err(Error {
                        message: format!("Cant emit a div for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            FactorExpressionChild::Mod(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a % b))),
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

                                let result = br.builder.build_float_rem(
                                    a.into_float_value(),
                                    b.into_float_value(),
                                    "div",
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
                                            "Cant emit an div for the types {} {}",
                                            a_type, b_type
                                        ),
                                        pos: Some(self.pos.clone()),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_unsigned_rem(
                                    a.into_int_value(),
                                    b.into_int_value(),
                                    "div",
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
                                    "Cant emit an div for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            }),
                        }
                    }

                    _ => Err(Error {
                        message: format!("Cant emit a div for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            FactorExpressionChild::UnaryExpression(compare) => compare.emit(ctx),
        }
    }
}

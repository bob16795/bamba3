use crate::errors::*;
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
    Xor(unary_expression::UnaryExpression, FactorExpression),
    UnaryExpression(unary_expression::UnaryExpression),
}

#[derive(Debug, Clone)]
pub struct FactorExpression {
    pos: FileRange,

    child: Box<FactorExpressionChild>,
}

impl Parsable for FactorExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();
        let first = unary_expression::UnaryExpression::parse(scn);

        if first.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::Percent).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                scn.set_checkpoint(start);
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
                scn.set_checkpoint(start);
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
                scn.set_checkpoint(start);
                return None;
            }

            return Some(FactorExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(FactorExpressionChild::Mul(first.unwrap(), next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Caret).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                scn.set_checkpoint(start);
                return None;
            }

            return Some(FactorExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(FactorExpressionChild::Xor(first.unwrap(), next.unwrap())),
            });
        }

        Some(FactorExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(FactorExpressionChild::UnaryExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for FactorExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            FactorExpressionChild::Mul(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a * b)),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "multiply".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            FactorExpressionChild::Div(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a / b)),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "divide".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            FactorExpressionChild::Xor(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(a ^ b)),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "xor".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            FactorExpressionChild::Mod(_l, _r) => Err(Error::BambaError {
                data: ErrorData::TodoError("visit mod expression".to_string()),
                pos: self.pos.clone(),
            }),
            FactorExpressionChild::UnaryExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            FactorExpressionChild::Mul(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a * b))),
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
                            (Value::DoubleType, Value::DoubleType) => {
                                let br = ctx.borrow();

                                let result = br.builder.build_float_mul(
                                    av.into_float_value(),
                                    bv.into_float_value(),
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
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "multiply".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_mul(
                                    av.into_int_value(),
                                    bv.into_int_value(),
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

                            _ => Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "multiply".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "multiply".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
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
                            (Value::DoubleType, Value::DoubleType) => {
                                let br = ctx.borrow();

                                let result = br.builder.build_float_div(
                                    av.into_float_value(),
                                    bv.into_float_value(),
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
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "divide".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_unsigned_div(
                                    av.into_int_value(),
                                    bv.into_int_value(),
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

                            _ => Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "divide".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "divide".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
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
                            (Value::DoubleType, Value::DoubleType) => {
                                let br = ctx.borrow();

                                let result = br.builder.build_float_rem(
                                    av.into_float_value(),
                                    bv.into_float_value(),
                                    "mod",
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
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "mod".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_int_unsigned_rem(
                                    av.into_int_value(),
                                    bv.into_int_value(),
                                    "mod",
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
                                    kind: "mod".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "mod".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            FactorExpressionChild::Xor(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt(a ^ b))),
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
                                            kind: "mod".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let result = br.builder.build_xor(
                                    av.into_int_value(),
                                    bv.into_int_value(),
                                    "xor",
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
                                    kind: "xor".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "mod".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            FactorExpressionChild::UnaryExpression(compare) => compare.emit(ctx),
        }
    }
}

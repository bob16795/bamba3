use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum CompareExpressionChild {
    Eql(term_expression::TermExpression, CompareExpression),
    NotEql(term_expression::TermExpression, CompareExpression),
    Less(term_expression::TermExpression, CompareExpression),
    Greater(term_expression::TermExpression, CompareExpression),
    TermExpression(term_expression::TermExpression),
}

#[derive(Debug, Clone)]
pub struct CompareExpression {
    pos: FileRange,

    child: Box<CompareExpressionChild>,
}

impl<'a> CompareExpression {
    fn build_cmp(
        &self,
        ctx: Rc<RefCell<NodeContext<'a>>>,
        a: &Value<'a>,
        b: &Value<'a>,
        p: IntPredicate,
    ) -> Result<Option<Value<'a>>, Error<'a>> {
        match (a.clone(), b.clone()) {
            (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Some(Value::ConstInt({
                match p {
                    IntPredicate::NE => {
                        if a != b {
                            1
                        } else {
                            0
                        }
                    }
                    IntPredicate::EQ => {
                        if a == b {
                            1
                        } else {
                            0
                        }
                    }
                    _ => todo!(),
                }
            }))),
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

                        let result = br.builder.build_float_compare(
                            match p {
                                IntPredicate::NE => FloatPredicate::UNE,
                                IntPredicate::EQ => FloatPredicate::UEQ,
                                IntPredicate::ULT => FloatPredicate::ULT,
                                IntPredicate::UGT => FloatPredicate::UGT,
                                _ => {
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "compare".to_string(),
                                            a: a.clone(),
                                            b: b.clone(),
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }
                            },
                            av.into_float_value(),
                            bv.into_float_value(),
                            "compare",
                        );

                        Ok(Some(Value::Value {
                            val: Rc::new(result.unwrap().into()),
                            kind: Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                ctx: ctx.clone(),

                                value: NodeV::Visited(Value::IntType {
                                    size: 1,
                                    signed: false,
                                }),
                            })),
                        }))
                    }
                    (Value::PointerType(atype), Value::PointerType(btype)) => {
                        let btype: Value = btype.clone().try_into()?;
                        if btype != atype.clone().try_into()? {
                            return Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "compare".to_string(),
                                    a: a.clone(),
                                    b: b.clone(),
                                },
                                pos: self.pos.clone(),
                            });
                        }

                        let br = ctx.borrow();

                        let result = br.builder.build_int_compare(
                            p,
                            av.into_pointer_value(),
                            bv.into_pointer_value(),
                            "compare",
                        );

                        Ok(Some(Value::Value {
                            val: Rc::new(result.unwrap().into()),
                            kind: Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                ctx: ctx.clone(),

                                value: NodeV::Visited(Value::IntType {
                                    size: 1,
                                    signed: false,
                                }),
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
                                    kind: "compare".to_string(),
                                    a: a.clone(),
                                    b: b.clone(),
                                },
                                pos: self.pos.clone(),
                            });
                        }

                        let br = ctx.borrow();

                        let result = br.builder.build_int_compare(
                            p,
                            av.into_int_value(),
                            bv.into_int_value(),
                            "compare",
                        );

                        Ok(Some(Value::Value {
                            val: Rc::new(result.unwrap().into()),
                            kind: Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                ctx: ctx.clone(),

                                value: NodeV::Visited(Value::IntType {
                                    size: 1,
                                    signed: false,
                                }),
                            })),
                        }))
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "compare".to_string(),
                            a: a.clone(),
                            b: b.clone(),
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }

            (a, b) => Ok(Some(Value::ConstBool(match p {
                IntPredicate::EQ => a == b,
                _ => todo!(),
            }))),
        }
    }
}

impl Parsable for CompareExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());
        let first = term_expression::TermExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Lt).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(CompareExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(CompareExpressionChild::Less(first.unwrap(), next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Gt).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(CompareExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(CompareExpressionChild::Greater(
                    first.unwrap(),
                    next.unwrap(),
                )),
            });
        }

        if scn.match_next(scanner::TokenKind::ExclaimEqual).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(CompareExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(CompareExpressionChild::NotEql(
                    first.unwrap(),
                    next.unwrap(),
                )),
            });
        }

        if scn.match_next(scanner::TokenKind::EqualEqual).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(CompareExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(CompareExpressionChild::Eql(first.unwrap(), next.unwrap())),
            });
        }

        Some(CompareExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(CompareExpressionChild::TermExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for CompareExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            CompareExpressionChild::NotEql(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a != b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "not equals".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            CompareExpressionChild::Eql(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a == b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "equals".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            CompareExpressionChild::Greater(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a > b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "greater than".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            CompareExpressionChild::Less(l, r) => {
                let a: Value = l.visit(ctx.clone())?.try_into()?;
                let b: Value = r.visit(ctx.clone())?.try_into()?;

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a < b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error::BambaError {
                        data: ErrorData::VisitBinaryOpError {
                            kind: "less than".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            CompareExpressionChild::TermExpression(compare) => compare.visit(ctx),
        }
    }
    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            CompareExpressionChild::NotEql(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                self.build_cmp(ctx, &a, &b, IntPredicate::NE)
            }
            CompareExpressionChild::Eql(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                self.build_cmp(ctx, &a, &b, IntPredicate::EQ)
            }
            CompareExpressionChild::Greater(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                self.build_cmp(ctx, &a, &b, IntPredicate::UGT)
            }
            CompareExpressionChild::Less(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                self.build_cmp(ctx, &a, &b, IntPredicate::ULT)
            }
            CompareExpressionChild::TermExpression(compare) => compare.emit(ctx),
        }
    }
}

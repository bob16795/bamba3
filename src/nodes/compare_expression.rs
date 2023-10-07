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
    ) -> Result<Option<Value<'a>>, Error> {
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

                        let result = br.builder.build_float_compare(
                            match p {
                                IntPredicate::NE => FloatPredicate::UNE,
                                IntPredicate::EQ => FloatPredicate::UEQ,
                                IntPredicate::ULT => FloatPredicate::ULT,
                                IntPredicate::UGT => FloatPredicate::UGT,
                                _ => {
                                    return Err(Error {
                                        message: format!("todo, {:?}", p),
                                        pos: Some(self.pos.clone()),
                                    });
                                }
                            },
                            a.into_float_value(),
                            b.into_float_value(),
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
                        let btype: Value = btype.clone().into();
                        if btype != atype.clone().into() {
                            return Err(Error {
                                message: format!(
                                    "Cant emit a compare for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            });
                        }

                        let br = ctx.borrow();

                        let result = br.builder.build_int_compare(
                            p,
                            a.into_pointer_value(),
                            b.into_pointer_value(),
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
                            return Err(Error {
                                message: format!(
                                    "Cant emit a compare for the types {} {}",
                                    a_type, b_type
                                ),
                                pos: Some(self.pos.clone()),
                            });
                        }

                        let br = ctx.borrow();

                        let result = br.builder.build_int_compare(
                            p,
                            a.into_int_value(),
                            b.into_int_value(),
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

                    _ => Err(Error {
                        message: format!("Cant emit a compare for the types {} {}", a_type, b_type),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }

            _ => Err(Error {
                message: format!("Cant emit a compare for the vals {} {}", a, b),
                pos: Some(self.pos.clone()),
            }),
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
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        match self.child.as_ref() {
            CompareExpressionChild::NotEql(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a != b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant visit a or for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            CompareExpressionChild::Eql(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a == b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant visit a or for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            CompareExpressionChild::Greater(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a > b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant visit a or for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            CompareExpressionChild::Less(l, r) => {
                let a: Value = l.visit(ctx.clone())?.into();
                let b: Value = r.visit(ctx.clone())?.into();

                match (a.clone(), b.clone()) {
                    (Value::ConstInt(a), Value::ConstInt(b)) => Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),

                        value: NodeV::Visited(Value::ConstInt(if a < b { 1 } else { 0 })),
                    }))),

                    _ => Err(Error {
                        message: format!("Cant visit a or for the types {} {}", a, b),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            CompareExpressionChild::TermExpression(compare) => compare.visit(ctx),
        }
    }
    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
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

use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use inkwell::types::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum UnaryExpressionChild {
    Ref(UnaryExpression),
    Deref(UnaryExpression),
    Not(UnaryExpression),
    CallExpression(call_expression::CallExpression),
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pos: FileRange,

    child: Box<UnaryExpressionChild>,
}

impl Parsable for UnaryExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Exclaim).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(UnaryExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(UnaryExpressionChild::Not(next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Ampersand).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(UnaryExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(UnaryExpressionChild::Ref(next.unwrap())),
            });
        }

        if scn.match_next(scanner::TokenKind::Star).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(UnaryExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(UnaryExpressionChild::Deref(next.unwrap())),
            });
        }

        let next = call_expression::CallExpression::parse(scn);

        if next.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        Some(UnaryExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(UnaryExpressionChild::CallExpression(next.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for UnaryExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        match self.child.as_ref() {
            UnaryExpressionChild::Not(_un) => Err(Error {
                message: format!("Todo: visit not expression"),
                pos: Some(self.pos.clone()),
            }),
            UnaryExpressionChild::Ref(_un) => Err(Error {
                message: format!("Todo: visit ref expression"),
                pos: Some(self.pos.clone()),
            }),
            UnaryExpressionChild::Deref(un) => {
                let child = un.visit(ctx.clone())?;
                child.borrow_mut().visit()?;

                match &child.clone().into() {
                    Value::Value { val, kind } => match kind.clone().into() {
                        Value::PointerType(p) => {
                            let load = {
                                let b = ctx.borrow();

                                let kind: BasicTypeEnum = match p.borrow_mut().get_type()?.as_ref()
                                {
                                    AnyTypeEnum::ArrayType(a) => a.clone().into(),
                                    AnyTypeEnum::FloatType(a) => a.clone().into(),
                                    AnyTypeEnum::IntType(a) => a.clone().into(),
                                    AnyTypeEnum::PointerType(a) => a.clone().into(),
                                    AnyTypeEnum::StructType(a) => a.clone().into(),
                                    a => {
                                        return Err(Error {
                                            message: format!("cant deref type ptr {}", a),
                                            pos: Some(self.pos.clone()),
                                        })
                                    }
                                };

                                b.builder
                                    .build_load(kind, val.into_pointer_value(), "deref")
                                    .unwrap()
                            };

                            Ok(Rc::new(RefCell::new(Node {
                                ctx: ctx.clone(),
                                pos: self.pos.clone(),
                                value: NodeV::Visited(Value::Value {
                                    val: load.into(),
                                    kind: p,
                                }),
                            })))
                        }
                        p => {
                            return Err(Error {
                                message: format!("cant deref type {}", p),
                                pos: Some(self.pos.clone()),
                            })
                        }
                    },
                    Value::VoidType
                    | Value::IntType { size: _, signed: _ }
                    | Value::Class {
                        children: _,
                        kind: _,
                        name: _,
                    }
                    | Value::PointerType(_)
                    | Value::Function { .. }
                    | Value::ArrayType { size: _, child: _ } => Ok(Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: self.pos.clone(),
                        value: NodeV::Visited(Value::PointerType(child.clone())),
                    }))),
                    _ => Err(Error {
                        message: format!("cant visit deref type {}", child.borrow()),
                        pos: Some(self.pos.clone()),
                    }),
                }
            }
            UnaryExpressionChild::CallExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        match self.child.as_ref() {
            UnaryExpressionChild::Not(un) => {
                let child = &un.emit(ctx.clone())?;
                match child {
                    Some(val) => match val {
                        Value::Value { val, kind } => match kind.clone().into() {
                            Value::IntType { size: _, signed: _ } => {
                                let b = ctx.borrow();

                                let load = b
                                    .builder
                                    .build_not(val.into_int_value(), "bitnot")
                                    .unwrap()
                                    .into();

                                Ok(Some(Value::Value {
                                    val: Rc::new(load),
                                    kind: kind.clone(),
                                }))
                            }
                            p => {
                                return Err(Error {
                                    message: format!("cant emit not type {}", p),
                                    pos: Some(self.pos.clone()),
                                })
                            }
                        },
                        _ => Err(Error {
                            message: format!("bad type {}", val),
                            pos: Some(self.pos.clone()),
                        }),
                    },
                    None => {
                        return Err(Error {
                            message: format!("Todo: emit ref expression"),
                            pos: Some(self.pos.clone()),
                        })
                    }
                }
            }
            UnaryExpressionChild::Ref(_un) => Err(Error {
                message: format!("Todo: emit ref expression"),
                pos: Some(self.pos.clone()),
            }),
            UnaryExpressionChild::Deref(un) => {
                let child = &un.emit(ctx.clone())?;

                match child {
                    Some(val) => match val {
                        Value::Value { val, kind } => match kind.clone().into() {
                            Value::PointerType(p) => {
                                let load = {
                                    let b = ctx.borrow();

                                    let kind: BasicTypeEnum =
                                        match p.borrow_mut().get_type()?.as_ref() {
                                            AnyTypeEnum::ArrayType(a) => a.clone().into(),
                                            AnyTypeEnum::FloatType(a) => a.clone().into(),
                                            AnyTypeEnum::IntType(a) => a.clone().into(),
                                            AnyTypeEnum::PointerType(a) => a.clone().into(),
                                            AnyTypeEnum::StructType(a) => a.clone().into(),
                                            a => {
                                                return Err(Error {
                                                    message: format!("cant deref type ptr {}", a),
                                                    pos: Some(self.pos.clone()),
                                                })
                                            }
                                        };

                                    b.builder
                                        .build_load(kind, val.into_pointer_value(), "deref")
                                        .unwrap()
                                };

                                Ok(Some(Value::Value {
                                    val: load.into(),
                                    kind: p,
                                }))
                            }
                            p => {
                                return Err(Error {
                                    message: format!("cant deref type {}", p),
                                    pos: Some(self.pos.clone()),
                                })
                            }
                        },
                        Value::VoidType
                        | Value::IntType { size: _, signed: _ }
                        | Value::Class {
                            children: _,
                            kind: _,
                            name: _,
                        }
                        | Value::PointerType(_)
                        | Value::ArrayType { size: _, child: _ } => {
                            Ok(Some(Value::PointerType(Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                ctx: ctx.clone(),
                                value: NodeV::Visited(val.clone()),
                            })))))
                        }
                        _ => Err(Error {
                            message: format!("bad type {}", val),
                            pos: Some(self.pos.clone()),
                        }),
                    },
                    None => {
                        return Err(Error {
                            message: format!("Todo: emit ref expression"),
                            pos: Some(self.pos.clone()),
                        })
                    }
                }
            }
            UnaryExpressionChild::CallExpression(compare) => compare.emit(ctx),
        }
    }
}

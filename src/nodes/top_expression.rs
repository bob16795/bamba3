use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use inkwell::types::*;
use inkwell::values::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum TopExpressionChild {
    Assignment(or_expression::OrExpression, TopExpression),
    OrExpression(or_expression::OrExpression),
    Prop(prop_expression::PropExpression),
}

#[derive(Debug, Clone)]
pub struct TopExpression {
    pos: FileRange,

    pub child: Box<TopExpressionChild>,
}

impl Parsable for TopExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let parsed = prop_expression::PropExpression::parse(scn);
        if parsed.is_some() {
            return Some(TopExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(TopExpressionChild::Prop(parsed.unwrap())),
            });
        }

        let first = or_expression::OrExpression::parse(scn);

        if first.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::Equal).is_some() {
            let next = Self::parse(scn);

            if next.is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(TopExpression {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(TopExpressionChild::Assignment(
                    first.unwrap(),
                    next.unwrap(),
                )),
            });
        }

        Some(TopExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(TopExpressionChild::OrExpression(first.unwrap())),
        })
    }
}

impl<'a> Visitable<'a> for TopExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            TopExpressionChild::Prop(i) => i.visit(ctx),
            TopExpressionChild::Assignment(_l, _r) => {
                let val = self.emit(ctx.clone())?;

                Ok(Rc::new(RefCell::new(Node {
                    pos: self.pos.clone(),
                    value: NodeV::Visited(val.unwrap()),
                    ctx,
                })))
            }
            TopExpressionChild::OrExpression(or) => or.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            TopExpressionChild::Prop(i) => i.emit(ctx),
            TopExpressionChild::Assignment(l, r) => {
                let a = l.emit(ctx.clone())?.unwrap();
                let b = r.emit(ctx.clone())?.unwrap();

                match (a.clone(), b.clone()) {
                    (
                        Value::Value {
                            val: av,
                            kind: a_type,
                        },
                        Value::ConstNull,
                    ) => {
                        let br = ctx.borrow();
                        let Value::PointerType(ty) = a_type.clone().try_into()? else {
                            return Err(Error::BambaError {
                                data: ErrorData::NoNullError {
                                    kind: a_type.try_into()?,
                                },
                                pos: self.pos.clone(),
                            });
                        };

                        let ty = ty.borrow_mut().get_type()?;

                        let zero: BasicValueEnum = match ty.as_ref() {
                            AnyTypeEnum::ArrayType(ty) => ty.const_zero().into(),
                            AnyTypeEnum::FloatType(ty) => ty.const_zero().into(),
                            AnyTypeEnum::IntType(ty) => ty.const_zero().into(),
                            AnyTypeEnum::PointerType(ty) => ty.const_zero().into(),
                            AnyTypeEnum::StructType(ty) => ty.const_zero().into(),
                            _ => {
                                return Err(Error::BambaError {
                                    data: ErrorData::NoNullError {
                                        kind: a_type.try_into()?,
                                    },
                                    pos: self.pos.clone(),
                                });
                            }
                        };

                        let _ = br.builder.build_store(av.into_pointer_value(), zero);

                        Ok(Some(a))
                    }
                    (
                        Value::Value {
                            val: av,
                            kind: _a_type,
                        },
                        Value::Tuple { children },
                    ) => {
                        let mut vals = Vec::new();

                        // TODO: check assignable

                        for c in children {
                            let Value::Value { val: c, kind: _ } = c.borrow().clone() else {
                                return Err(Error::BambaError {
                                    data: ErrorData::NoValueError,
                                    pos: self.pos.clone(),
                                });
                            };

                            vals.push(c.as_ref().clone());
                        }

                        let br = ctx.borrow();

                        let val = br.context.const_struct(&vals, false);

                        let _ = br.builder.build_store(av.into_pointer_value(), val);

                        Ok(Some(a))
                    }
                    (
                        Value::Value {
                            val: av,
                            kind: _a_type,
                        },
                        Value::Function { .. },
                    ) => {
                        let mut b = Node {
                            ctx: ctx.clone(),
                            pos: self.pos.clone(),
                            value: NodeV::Visited(b),
                        };

                        let val = b.get_impl(None);

                        let br = ctx.borrow();

                        let _ = br.builder.build_store(
                            av.into_pointer_value(),
                            val.unwrap().func.as_global_value(),
                        );

                        Ok(Some(a))
                    }
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
                        let a_type = &a_type.try_into()?;
                        let b_type = &b_type.try_into()?;

                        match (&a_type, &b_type) {
                            (Value::PointerType(kind), Value::Class { .. }) => {
                                let kind_val: Value = kind.clone().try_into()?;
                                if kind_val != b_type.clone() {
                                    return Err(Error::BambaError {
                                        data: ErrorData::EmitBinaryOpError {
                                            kind: "assign".to_string(),
                                            a,
                                            b,
                                        },
                                        pos: self.pos.clone(),
                                    });
                                }

                                let br = ctx.borrow();

                                let _ = br
                                    .builder
                                    .build_store(av.into_pointer_value(), bv.into_struct_value());

                                Ok(Some(a))
                            }
                            (Value::PointerType(_kind), Value::ArrayType { size: _, child: _ }) => {
                                let br = ctx.borrow();

                                let _ = br
                                    .builder
                                    .build_store(av.into_pointer_value(), bv.into_array_value());

                                Ok(Some(a))
                            }
                            (Value::PointerType(_kind), Value::DoubleType) => {
                                let br = ctx.borrow();

                                let _ = br
                                    .builder
                                    .build_store(av.into_pointer_value(), bv.into_float_value());

                                Ok(Some(a))
                            }
                            (Value::PointerType(_kind), Value::IntType { size: _, signed: _ }) => {
                                let br = ctx.borrow();

                                let _ = br
                                    .builder
                                    .build_store(av.into_pointer_value(), bv.into_int_value());

                                Ok(Some(a))
                            }
                            (Value::PointerType(_kind), Value::PointerType(_other)) => {
                                let br = ctx.borrow();

                                let _ = br
                                    .builder
                                    .build_store(av.into_pointer_value(), bv.into_pointer_value());

                                Ok(Some(a))
                            }
                            _ => Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "assign".to_string(),
                                    a,
                                    b,
                                },
                                pos: self.pos.clone(),
                            }),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::EmitBinaryOpError {
                            kind: "assign".to_string(),
                            a,
                            b,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            TopExpressionChild::OrExpression(or) => or.emit(ctx),
        }
    }
}

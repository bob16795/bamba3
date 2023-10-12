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
pub enum CallExpressionChild {
    Call {
        func: CallExpression,
        params: Vec<top_expression::TopExpression>,
    },
    Access {
        parent: CallExpression,
        prop: String,
    },
    Index {
        parent: CallExpression,
        index: Option<top_expression::TopExpression>,
    },
    PrimaryExpression(primary_expression::PrimaryExpression),
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub pos: FileRange,

    pub child: Box<CallExpressionChild>,
}

impl Parsable for CallExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();
        let next = primary_expression::PrimaryExpression::parse(scn);

        if next.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let mut next = CallExpression {
            pos: (start.1..scn.pos.clone()).into(),

            child: Box::new(CallExpressionChild::PrimaryExpression(next.unwrap())),
        };

        let mut added = true;

        while added {
            added = false;

            let sub_start = scn.get_checkpoint();
            if scn.match_next(scanner::TokenKind::Dot).is_some() {
                let prop = scn.match_next(scanner::TokenKind::Identifier);
                if prop.is_none() {
                    scn.set_checkpoint(sub_start.clone());
                } else {
                    next = CallExpression {
                        pos: (next.pos.start.clone()..scn.pos.clone()).into(),

                        child: Box::new(CallExpressionChild::Access {
                            parent: next,
                            prop: prop.unwrap().value,
                        }),
                    };

                    added = true;
                }
            }

            if scn.match_next(scanner::TokenKind::LeftBracket).is_some() {
                let index = top_expression::TopExpression::parse(scn);

                if scn.match_next(scanner::TokenKind::RightBracket).is_none() {
                    scn.set_checkpoint(sub_start.clone());
                } else {
                    next = CallExpression {
                        pos: (next.pos.start.clone()..scn.pos.clone()).into(),

                        child: Box::new(CallExpressionChild::Index {
                            parent: next,
                            index,
                        }),
                    };

                    added = true;
                }
            }

            if scn.match_next(scanner::TokenKind::LeftParen).is_some() {
                let mut params = Vec::new();

                let mut bad = false;

                while !bad && scn.match_next(scanner::TokenKind::RightParen).is_none() {
                    let def = top_expression::TopExpression::parse(scn);
                    if def.is_some() {
                        params.push(def.unwrap());
                        if scn.match_next(scanner::TokenKind::Comma).is_none() {
                            if scn.match_next(scanner::TokenKind::RightParen).is_none() {
                                scn.set_checkpoint(sub_start);
                                bad = true;
                            }

                            break;
                        }
                    } else {
                        if scn.match_next(scanner::TokenKind::RightParen).is_none() {
                            scn.set_checkpoint(sub_start.clone());
                            bad = true;
                        }

                        break;
                    }
                }

                if !bad {
                    next = CallExpression {
                        pos: (next.pos.start.clone()..scn.pos.clone()).into(),

                        child: Box::new(CallExpressionChild::Call { func: next, params }),
                    };

                    added = true;
                }
            }
        }

        Some(next)
    }
}

impl<'a> Visitable<'a> for CallExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            CallExpressionChild::Index {
                parent: child,
                index,
            } => {
                let child = child.visit(ctx.clone())?;

                match child.clone().try_into()? {
                    Value::ConstString(_) => {
                        todo!();
                    }
                    Value::Tuple { children } => {
                        let Some(idx) = index else {
                            return Err(Error::BambaError {
                                data: ErrorData::TodoError("lol".to_string()),
                                pos: self.pos.clone(),
                            });
                        };

                        let index = idx.visit(ctx.clone())?;

                        match index.try_into()? {
                            Value::ConstInt(i) => Ok(Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                value: NodeV::Visited(children[i as usize].borrow().clone()),
                                ctx: ctx.clone(),
                            }))),
                            _ => Err(Error::BambaError {
                                data: ErrorData::TodoError("lol".to_string()),
                                pos: self.pos.clone(),
                            }),
                        }
                    }
                    Value::FloatType
                    | Value::DoubleType
                    | Value::ClassType
                    | Value::TypeType
                    | Value::ArrayType { .. }
                    | Value::PointerType(_)
                    | Value::Class { .. }
                    | Value::IntType { .. } => {
                        let Some(idx) = index else {
                            return Ok(Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                value: NodeV::Visited(Value::ArrayType { child, size: None }),
                                ctx: ctx.clone(),
                            })));
                        };

                        let index = idx.visit(ctx.clone())?;

                        match index.try_into()? {
                            Value::ConstInt(i) => Ok(Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                value: NodeV::Visited(Value::ArrayType {
                                    child,
                                    size: Some(i as u32),
                                }),
                                ctx: ctx.clone(),
                            }))),
                            _ => Err(Error::BambaError {
                                data: ErrorData::TodoError("lol".to_string()),
                                pos: self.pos.clone(),
                            }),
                        }
                    }
                    _ => match index {
                        None => {
                            return Err(Error::BambaError {
                                data: ErrorData::VisitUnaryOpError {
                                    kind: "index".to_string(),
                                    a: child.try_into()?,
                                },
                                pos: self.pos.clone(),
                            })
                        }
                        Some(idx) => {
                            let index = idx.visit(ctx.clone())?;

                            return Err(Error::BambaError {
                                data: ErrorData::VisitBinaryOpError {
                                    kind: "index".to_string(),
                                    a: child.try_into()?,
                                    b: index.try_into()?,
                                },
                                pos: self.pos.clone(),
                            });
                        }
                    },
                }
            }
            CallExpressionChild::Access { parent, prop } => {
                let parent = parent.visit(ctx)?;
                let parent = &mut parent.borrow_mut();

                Ok(parent.get_child(prop.to_string())?)
            }
            CallExpressionChild::Call { func, params } => {
                let func = func.emit(ctx.clone())?.unwrap();

                match func.clone().into() {
                    Value::BuiltinFunc(BuiltinFunc::Print) => {
                        let p0 = params[0].visit(ctx.clone())?;

                        let name = match p0.try_into()? {
                            Value::ConstString(s) => s,
                            kind => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        println!("PRINT: {}", name);

                        Ok(Rc::new(RefCell::new(Node {
                            pos: self.pos.clone(),
                            ctx: ctx.clone(),
                            value: NodeV::Visited(Value::VoidType),
                        })))
                    }
                    Value::BuiltinFunc(BuiltinFunc::GetProp) => {
                        let mut emitted = params[0].emit(ctx.clone())?.unwrap();

                        let p1 = params[1].visit(ctx.clone())?;

                        let name = match p1.try_into()? {
                            Value::ConstString(s) => s,
                            kind => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        Ok(Rc::new(RefCell::new(Node {
                            pos: self.pos.clone(),
                            ctx: ctx.clone(),
                            value: NodeV::Visited(emitted.get_child(
                                name,
                                ctx.clone(),
                                self.pos.clone(),
                            )?),
                        })))
                    }
                    Value::BuiltinFunc(BuiltinFunc::AddDef) => {
                        let p0 = params[0].visit(ctx.clone())?;

                        let Value::Class { children, .. } = p0.clone().try_into()? else {
                            return Err(Error::BambaError {
                                data: ErrorData::ClassError {
                                    kind: p0.try_into()?,
                                },
                                pos: self.pos.clone(),
                            });
                        };

                        let p1 = params[1].visit(ctx.clone())?;

                        let name = match p1.try_into()? {
                            Value::ConstString(s) => s,
                            kind => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        let p2 = params[2].emit(ctx.clone())?;

                        children.borrow_mut().insert(
                            name.clone(),
                            (
                                Rc::new(RefCell::new(None)),
                                Rc::new(RefCell::new(Node {
                                    ctx: ctx.clone(),
                                    pos: self.pos.clone(),

                                    value: NodeV::Visited(p2.clone().unwrap()),
                                })),
                            ),
                        );

                        match p2.unwrap() {
                            Value::Prop { .. } => {
                                p0.borrow_mut().clone().regen(name)?;
                            }
                            _ => {}
                        }

                        Ok(Rc::new(RefCell::new(Node {
                            pos: self.pos.clone(),
                            ctx: ctx.clone(),
                            value: NodeV::Visited(Value::VoidType),
                        })))
                    }

                    _ => {
                        let mut func = Node {
                            pos: self.pos.clone(),
                            ctx: ctx.clone(),
                            value: NodeV::Visited(func),
                        };

                        func.visit()?;

                        let mut new_params = vec![];

                        for param in params {
                            let param = param.visit(ctx.clone())?;
                            param.borrow_mut().visit()?;

                            new_params.push(param);
                        }

                        Ok(func.clone().call(new_params)?)
                    }
                }
            }
            CallExpressionChild::PrimaryExpression(compare) => compare.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            CallExpressionChild::Index { parent, index } => {
                let Some(idx) = index else {
                    return Ok(Some(self.visit(ctx)?.try_into()?));
                };

                let parent = parent.emit(ctx.clone())?;

                let index = idx.emit(ctx.clone())?;

                match (parent.clone().unwrap(), index.clone().unwrap()) {
                    (Value::Value { val, kind, .. }, Value::Value { val: iv, .. }) => {
                        let cb = ctx.clone();
                        let cb = cb.borrow();

                        let k: Value = kind.clone().try_into()?;
                        let Value::PointerType(k) = k else {
                            return Err(Error::BambaError {
                                data: ErrorData::EmitBinaryOpError {
                                    kind: "index".to_string(),
                                    a: parent.unwrap(),
                                    b: index.unwrap(),
                                },
                                pos: self.pos.clone(),
                            });
                        };

                        let k = k.borrow().clone().get_type()?;

                        let k: BasicTypeEnum = match k.as_ref() {
                            AnyTypeEnum::ArrayType(t) => t.get_element_type().clone().into(),
                            AnyTypeEnum::PointerType(t) => t.clone().into(),
                            AnyTypeEnum::StructType(t) => t.clone().into(),
                            _ => {
                                return Err(Error::BambaError {
                                    data: ErrorData::EmitBinaryOpError {
                                        kind: "index".to_string(),
                                        a: parent.unwrap(),
                                        b: index.unwrap(),
                                    },
                                    pos: self.pos.clone(),
                                });
                            }
                        };

                        let Value::PointerType(kind_val) = kind.clone().try_into()? else {
                            todo!();
                        };

                        let Value::ArrayType {
                            child: kind,
                            size: _,
                        } = kind_val.clone().try_into()?
                        else {
                            let Value::Class {
                                children, name: _, ..
                            } = kind_val.clone().try_into()?
                            else {
                                let v: Value = kind_val.try_into()?;
                                return Err(Error::BambaError {
                                    data: ErrorData::TodoError(format!("index for {}", v)),
                                    pos: self.pos.clone(),
                                });
                            };

                            let func = {
                                let children = children.borrow();

                                match children.get("[]") {
                                    Some(v) => v.clone(),
                                    None => {
                                        return Err(Error::BambaError {
                                            data: ErrorData::NoChildError {
                                                child: "".to_string(),
                                                parent: kind_val.try_into()?,
                                            },
                                            pos: self.pos.clone(),
                                        })
                                    }
                                }
                            };

                            let mut p = vec![
                                parent.clone().unwrap().into(),
                                index.clone().unwrap().into(),
                            ];

                            drop(cb);

                            return Ok(Some(func.1.borrow().clone().emit_call(&mut p)?));
                        };

                        let val: BasicValueEnum = unsafe {
                            cb.builder
                                .build_in_bounds_gep(
                                    k,
                                    val.into_pointer_value(),
                                    &[iv.into_int_value()],
                                    &format!("AnonIndex"),
                                )
                                .unwrap()
                        }
                        .into();

                        Ok(Some(Value::Value {
                            val: Rc::new(val),
                            kind: Rc::new(RefCell::new(Node {
                                pos: self.pos.clone(),
                                ctx: ctx.clone(),
                                value: NodeV::Visited(Value::PointerType(kind)),
                            })),
                            dropable: false,
                        }))
                    }
                    (Value::Value { val, kind, .. }, Value::ConstInt(i)) => {
                        let cb = ctx.clone();
                        let cb = cb.borrow();

                        let val: BasicValueEnum = unsafe {
                            cb.builder
                                .build_gep(
                                    kind.clone().borrow().clone().get_type()?.into_array_type(),
                                    val.into_pointer_value(),
                                    &[cb.context
                                        .custom_width_int_type(32)
                                        .const_int(i as u64, false)],
                                    &format!("AnonIndex"),
                                )
                                .unwrap()
                        }
                        .into();

                        Ok(Some(Value::Value {
                            val: Rc::new(val),
                            kind,
                            dropable: false,
                        }))
                    }
                    _ => Ok(Some(self.visit(ctx.clone())?.try_into()?)),
                }
            }
            CallExpressionChild::Access { parent, prop } => {
                let parent = parent.emit(ctx.clone())?.unwrap();

                let mut parent = Node {
                    pos: self.pos.clone(),
                    ctx: ctx.clone(),
                    value: NodeV::Visited(parent),
                };

                parent.visit()?;

                Ok(Some(parent.get_child(prop.to_string())?.try_into()?))
            }
            CallExpressionChild::Call { func, params } => {
                let func = func.emit(ctx.clone())?.unwrap();

                match func.clone().into() {
                    Value::BuiltinFunc(BuiltinFunc::GetProp) => {
                        let mut emitted = params[0].emit(ctx.clone())?.unwrap();

                        let p1 = params[1].visit(ctx.clone())?;

                        let name = match p1.try_into()? {
                            Value::ConstString(s) => s,
                            v => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind: v },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        Ok(Some(emitted.get_child(
                            name,
                            ctx.clone(),
                            self.pos.clone(),
                        )?))
                    }
                    Value::BuiltinFunc(BuiltinFunc::SetName) => {
                        let p0 = params[0].visit(ctx.clone())?;

                        let p1 = params[1].visit(ctx.clone())?;

                        let name = match p1.try_into()? {
                            Value::ConstString(s) => s,
                            v => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind: v },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        p0.borrow_mut().set_name(name)?;

                        Ok(Some(p0.try_into()?))
                    }
                    Value::BuiltinFunc(BuiltinFunc::Error) => {
                        let p0 = params[0].visit(ctx.clone())?;

                        let value = match p0.try_into()? {
                            Value::ConstString(s) => s,
                            kind => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        return Err(Error::BambaError {
                            data: ErrorData::ErrorFunctionCall { value },
                            pos: self.pos.clone(),
                        });
                    }
                    Value::BuiltinFunc(BuiltinFunc::AddDef) => {
                        let p0 = params[0].visit(ctx.clone())?;

                        let Value::Class { children, .. } = p0.clone().try_into()? else {
                            return Err(Error::BambaError {
                                data: ErrorData::ClassError {
                                    kind: p0.try_into()?,
                                },
                                pos: self.pos.clone(),
                            });
                        };

                        let p1 = params[1].visit(ctx.clone())?;

                        let name = match p1.try_into()? {
                            Value::ConstString(s) => s,
                            kind => {
                                return Err(Error::BambaError {
                                    data: ErrorData::StringError { kind },
                                    pos: self.pos.clone(),
                                })
                            }
                        };

                        let mut p2 = params[2].emit(ctx.clone())?.unwrap();

                        p2.set_name(name.clone())?;

                        children.borrow_mut().insert(
                            name.clone(),
                            (
                                Rc::new(RefCell::new(None)),
                                Rc::new(RefCell::new(Node {
                                    ctx: ctx.clone(),
                                    pos: self.pos.clone(),

                                    value: NodeV::Visited(p2.clone()),
                                })),
                            ),
                        );

                        match p2.into() {
                            Value::Prop { .. } => {
                                p0.borrow_mut().clone().regen(name)?;
                            }
                            _ => {}
                        }

                        Ok(Some(Value::VoidType))
                    }

                    _ => {
                        let mut func = Node {
                            pos: self.pos.clone(),
                            ctx: ctx.clone(),
                            value: NodeV::Visited(func),
                        };

                        func.visit()?;

                        let mut p = Vec::new();

                        for param in params {
                            let em = param.emit(ctx.clone())?;

                            p.push(em.unwrap());
                        }

                        Ok(Some(func.emit_call(&mut p)?))
                    }
                }
            }
            CallExpressionChild::PrimaryExpression(compare) => compare.emit(ctx),
        }
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        match self.child.as_ref() {
            CallExpressionChild::Call { func, params } => {
                for c in params {
                    if c.uses(name)? {
                        return Ok(true);
                    }
                }

                func.uses(name)
            }
            CallExpressionChild::Access { parent, .. } => parent.uses(name),
            CallExpressionChild::Index { parent, index } => {
                Ok(parent.uses(name)? || (index.is_some() && index.clone().unwrap().uses(name)?))
            }
            CallExpressionChild::PrimaryExpression(p) => p.uses(name),
        }
    }
}

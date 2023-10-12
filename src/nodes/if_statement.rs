use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use inkwell::values::*;
use inkwell::IntPredicate;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct IfStatement {
    pos: FileRange,

    expr: top_expression::TopExpression,
    child: statement::Statement,
    other: Option<statement::Statement>,
}

impl Parsable for IfStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::If).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::LeftParen).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let expr = top_expression::TopExpression::parse(scn);

        if expr.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::RightParen).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let parsed = statement::Statement::parse(scn);
        if parsed.is_some() {
            if scn.match_next(scanner::TokenKind::Else).is_some() {
                let other = statement::Statement::parse(scn);

                if other.is_none() {
                    scn.set_checkpoint(start);
                    return None;
                }

                return Some(IfStatement {
                    pos: (start.1..scn.pos.clone()).into(),

                    expr: expr.unwrap(),
                    child: parsed.unwrap(),
                    other,
                });
            }

            return Some(IfStatement {
                pos: (start.1..scn.pos.clone()).into(),

                expr: expr.unwrap(),
                child: parsed.unwrap(),
                other: None,
            });
        }

        scn.set_checkpoint(start);
        None
    }
}

impl<'a> Visitable<'a> for IfStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let cond = self.expr.visit(ctx.clone())?;

        match cond.try_into()? {
            Value::ConstInt(v) => {
                if v != 0 {
                    self.child.visit(ctx.clone())
                } else if self.other.is_some() {
                    self.other.clone().unwrap().visit(ctx.clone())
                } else {
                    Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),
                        value: NodeV::Visited(Value::VoidType),
                    })))
                }
            }
            Value::ConstBool(v) => {
                if v {
                    self.child.visit(ctx.clone())
                } else if self.other.is_some() {
                    self.other.clone().unwrap().visit(ctx.clone())
                } else {
                    Ok(Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),
                        value: NodeV::Visited(Value::VoidType),
                    })))
                }
            }
            _ => Err(Error::BambaError {
                pos: self.pos.clone(),
                data: ErrorData::NoValueError,
            }),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let tmp = self.expr.emit(ctx.clone())?;
        return match tmp.clone() {
            None => Err(Error::BambaError {
                pos: self.pos.clone(),
                data: ErrorData::NoValueError,
            }),
            Some(Value::Value { val, .. }) => {
                let (body_bb, else_bb, merge_bb) = {
                    let context = &ctx.borrow();

                    let body_bb = context
                        .context
                        .append_basic_block(context.func.unwrap(), "ifBody");

                    let else_bb = context
                        .context
                        .append_basic_block(context.func.unwrap(), "ifElse");

                    let merge_bb = context
                        .context
                        .append_basic_block(context.func.unwrap(), "ifMerge");

                    (body_bb, else_bb, merge_bb)
                };

                match &val.as_ref() {
                    BasicValueEnum::IntValue(i) => {
                        let body_ret: bool;

                        {
                            let context = &ctx.borrow();
                            let v = context
                                .builder
                                .build_int_compare(
                                    IntPredicate::NE,
                                    (*i).into(),
                                    i.get_type().const_zero(),
                                    "ifCondition",
                                )
                                .unwrap();

                            _ = context
                                .builder
                                .build_conditional_branch(v, body_bb, else_bb);

                            context.builder.position_at_end(body_bb);
                        }

                        self.child.emit(ctx.clone())?;

                        {
                            let context = &ctx.borrow();
                            body_ret = *context.returned.clone().unwrap().borrow();

                            *context.returned.clone().unwrap().borrow_mut() = false;

                            if !body_ret {
                                _ = context.builder.build_unconditional_branch(merge_bb);
                            }

                            context.builder.position_at_end(else_bb);
                        }

                        if self.other.is_some() {
                            self.other.clone().unwrap().emit(ctx.clone())?;
                        }

                        {
                            let context = &ctx.borrow();

                            let else_ret = *context.returned.clone().unwrap().borrow();

                            if !else_ret {
                                _ = context.builder.build_unconditional_branch(merge_bb);
                            }

                            context.builder.position_at_end(merge_bb);

                            *context.returned.clone().unwrap().borrow_mut() = else_ret && body_ret;
                        }

                        Ok(None)
                    }
                    _ => Err(Error::BambaError {
                        pos: self.pos.clone(),
                        data: ErrorData::ZeroCompareError {
                            value: tmp.unwrap(),
                        },
                    }),
                }
            }
            Some(Value::ConstInt(v)) => {
                if v != 0 {
                    self.child.emit(ctx.clone())
                } else if self.other.is_some() {
                    self.other.clone().unwrap().emit(ctx.clone())
                } else {
                    Ok(None)
                }
            }
            Some(Value::ConstBool(v)) => {
                if v {
                    self.child.emit(ctx.clone())
                } else if self.other.is_some() {
                    self.other.clone().unwrap().emit(ctx.clone())
                } else {
                    Ok(None)
                }
            }
            _ => Err(Error::BambaError {
                pos: self.pos.clone(),
                data: ErrorData::ZeroCompareError {
                    value: tmp.unwrap(),
                },
            }),
        };
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        Ok(self.expr.uses(name)?
            || self.child.uses(name)?
            || (self.other.is_some() && self.other.clone().unwrap().uses(name)?))
    }
}

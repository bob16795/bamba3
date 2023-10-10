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
pub struct WhileStatement {
    pos: FileRange,

    expr: top_expression::TopExpression,
    child: statement::Statement,
}

impl Parsable for WhileStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::While).is_none() {
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
            return Some(WhileStatement {
                pos: (start.1..scn.pos.clone()).into(),

                expr: expr.unwrap(),
                child: parsed.unwrap(),
            });
        }

        scn.set_checkpoint(start);
        None
    }
}

impl<'a> Visitable<'a> for WhileStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        loop {
            let cond = self.expr.visit(ctx.clone())?;
            match cond.try_into()? {
                Value::ConstBool(b) => {
                    if !b {
                        break;
                    }
                }
                cond => {
                    return Err(Error::BambaError {
                        pos: self.pos.clone(),
                        data: ErrorData::ZeroCompareError { value: cond },
                    })
                }
            }

            self.child.visit(ctx.clone())?;
        }

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(Value::VoidType),
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let (head_bb, body_bb, merge_bb) = {
            let context = &ctx.borrow();

            let head_bb = context
                .context
                .append_basic_block(context.func.unwrap(), "whileHead");

            let body_bb = context
                .context
                .append_basic_block(context.func.unwrap(), "whileBody");

            let merge_bb = context
                .context
                .append_basic_block(context.func.unwrap(), "whileMerge");

            let _ = context.builder.build_unconditional_branch(head_bb);
            context.builder.position_at_end(head_bb);

            (head_bb, body_bb, merge_bb)
        };

        let tmp = self.expr.emit(ctx.clone())?;
        return match tmp {
            None => Err(Error::BambaError {
                pos: self.pos.clone(),
                data: ErrorData::NoValueError,
            }),
            Some(Value::Value { val, kind }) => match &val.clone().as_ref() {
                BasicValueEnum::IntValue(i) => {
                    let old_break = {
                        let context = &mut ctx.borrow_mut();
                        let v = context
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                (*i).into(),
                                i.get_type().const_zero(),
                                "whileCondition",
                            )
                            .unwrap();

                        _ = context
                            .builder
                            .build_conditional_branch(v, body_bb, merge_bb);

                        context.builder.position_at_end(body_bb);

                        let old_break = context.break_pos;

                        context.break_pos = Some(merge_bb);

                        old_break
                    };

                    self.child.emit(ctx.clone())?;

                    {
                        let context = &mut ctx.borrow_mut();

                        _ = context.builder.build_unconditional_branch(head_bb);

                        context.builder.position_at_end(merge_bb);

                        context.break_pos = old_break;
                    }

                    Ok(None)
                }
                _ => Err(Error::BambaError {
                    pos: self.pos.clone(),
                    data: ErrorData::ZeroCompareError {
                        value: kind.try_into()?,
                    },
                }),
            },
            Some(tmp) => Err(Error::BambaError {
                pos: self.pos.clone(),
                data: ErrorData::ZeroCompareError { value: tmp.clone() },
            }),
        };
    }
}

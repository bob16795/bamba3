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
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::If).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::LeftParen).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let expr = top_expression::TopExpression::parse(scn);

        if expr.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::RightParen).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let parsed = statement::Statement::parse(scn);
        if parsed.is_some() {
            if scn.match_next(scanner::TokenKind::Else).is_some() {
                let other = statement::Statement::parse(scn);

                if other.is_none() {
                    (scn.slice, scn.pos) = start;
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

        (scn.slice, scn.pos) = start;
        None
    }
}

impl<'a> Visitable<'a> for IfStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        let cond = self.expr.visit(ctx.clone())?;

        match cond.into() {
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
            v => Err(Error {
                message: format!("expected value in if cond, found {}", v),
                pos: Some(self.pos.clone()),
            }),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
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

        let tmp = self.expr.emit(ctx.clone())?;
        return match tmp {
            None => Err(Error {
                message: format!("expected value"),
                pos: Some(self.pos.clone()),
            }),
            Some(Value::Value { val, kind: _ }) => match &val.as_ref() {
                BasicValueEnum::IntValue(i) => {
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

                        _ = context.builder.build_unconditional_branch(merge_bb);

                        context.builder.position_at_end(else_bb);
                    }

                    if self.other.is_some() {
                        self.other.clone().unwrap().emit(ctx.clone())?;
                    }

                    {
                        let context = &ctx.borrow();

                        _ = context.builder.build_unconditional_branch(merge_bb);

                        context.builder.position_at_end(merge_bb);
                    }

                    Ok(None)
                }
                _ => Err(Error {
                    message: format!("cant check if {} is zero", val),
                    pos: Some(self.pos.clone()),
                }),
            },
            _ => Err(Error {
                message: format!("cant check if {} is zero", tmp.unwrap()),
                pos: Some(self.pos.clone()),
            }),
        };
    }
}

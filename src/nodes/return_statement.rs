use crate::errors::*;
use crate::nodes::top_expression;
use crate::parser::Parsable;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pos: FileRange,

    expr: Option<top_expression::TopExpression>,
}
impl Parsable for ReturnStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Return).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::SemiColon).is_some() {
            return Some(ReturnStatement {
                pos: (start.1..scn.pos.clone()).into(),

                expr: None,
            });
        }

        let parsed = top_expression::TopExpression::parse(scn);

        if scn.match_next(scanner::TokenKind::SemiColon).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        return Some(ReturnStatement {
            pos: (start.1..scn.pos.clone()).into(),

            expr: parsed,
        });
    }
}

impl<'a> Visitable<'a> for ReturnStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match &self.expr {
            None => Ok(Rc::new(RefCell::new(Node {
                pos: self.pos.clone(),
                ctx: ctx.clone(),

                value: NodeV::Visited(Value::VoidType),
            }))),
            Some(expr) => expr.visit(ctx.clone()),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        {
            let ctxb = ctx.borrow();

            for (name, (id, fv)) in &mut ctxb.locals.borrow().iter() {
                if id.borrow().is_none() {
                    continue;
                };

                let fv = fv.try_borrow();

                let Ok(sv) = fv else {
                    println!("? {}", name);
                    continue;
                };

                if self.uses(name)? {
                    continue;
                }

                let v = sv.clone();

                drop(sv);

                if let NodeV::Emited(v, _) = &v.value {
                    v.emit_drop(self.pos.clone())?;
                }
            }
        }

        match &self.expr {
            None => {
                let ctx_borrow = ctx.borrow();

                let _ = ctx_borrow.builder.build_return(None);

                *ctx.borrow().returned.clone().unwrap().borrow_mut() = true;

                Ok(None)
            }
            Some(expr) => {
                let out = expr.emit(ctx.clone())?;

                match out {
                    Some(Value::Value { val, .. }) => {
                        let ctx_borrow = ctx.borrow();

                        let _ = ctx_borrow.builder.build_return(Some(val.as_ref()));

                        *ctx.borrow().returned.clone().unwrap().borrow_mut() = true;

                        Ok(None)
                    }
                    None | Some(Value::VoidType) => {
                        let ctx_borrow = ctx.borrow();

                        let _ = ctx_borrow.builder.build_return(None);

                        *ctx.borrow().returned.clone().unwrap().borrow_mut() = true;

                        Ok(None)
                    }
                    Some(kind) => {
                        return Err(Error::BambaError {
                            data: ErrorData::NoEmitReturnTypeError { kind },
                            pos: self.pos.clone(),
                        });
                    }
                }
            }
        }
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        Ok(self.expr.is_some() && self.expr.clone().unwrap().uses(name)?)
    }
}

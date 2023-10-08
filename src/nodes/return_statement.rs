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
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Return).is_none() {
            (scn.slice, scn.pos) = start;
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
            (scn.slice, scn.pos) = start;
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
        match &self.expr {
            None => {
                let ctx_borrow = ctx.borrow();

                let _ = ctx_borrow.builder.build_return(None);

                Ok(None)
            }
            Some(expr) => {
                let out = expr.emit(ctx.clone())?;

                match out {
                    Some(Value::Value { val, kind: _ }) => {
                        let ctx_borrow = ctx.borrow();

                        let _ = ctx_borrow.builder.build_return(Some(val.as_ref()));

                        Ok(None)
                    }
                    None | Some(Value::VoidType) => {
                        let ctx_borrow = ctx.borrow();

                        let _ = ctx_borrow.builder.build_return(None);

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
}

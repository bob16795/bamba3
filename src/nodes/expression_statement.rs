use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pos: FileRange,

    expr: top_expression::TopExpression,
}

impl Parsable for ExpressionStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let parsed = top_expression::TopExpression::parse(scn);
        if parsed.is_some() {
            if scn.match_next(scanner::TokenKind::SemiColon).is_none() {
                (scn.slice, scn.pos) = start;
                return None;
            }

            return Some(ExpressionStatement {
                pos: (start.1..scn.pos.clone()).into(),

                expr: parsed.unwrap(),
            });
        }

        (scn.slice, scn.pos) = start;
        None
    }
}

impl<'a> Visitable<'a> for ExpressionStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        self.expr.visit(ctx.clone())?;

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::VoidType),
            ctx,
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        self.expr.emit(ctx)
    }
}

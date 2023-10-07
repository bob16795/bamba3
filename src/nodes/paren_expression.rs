use crate::nodes::*;
use crate::parser::*;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParenExpression {
    child: top_expression::TopExpression,
}

impl Parsable for ParenExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::LeftParen).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let child = top_expression::TopExpression::parse(scn);

        if child.is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::RightParen).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        Some(ParenExpression {
            child: child.unwrap(),
        })
    }
}

impl<'a> Visitable<'a> for ParenExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        self.child.visit(ctx.clone())
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        self.child.emit(ctx.clone())
    }
}

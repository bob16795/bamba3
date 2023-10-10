use crate::errors::*;
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
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::LeftParen).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let child = top_expression::TopExpression::parse(scn);

        if child.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::RightParen).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        Some(ParenExpression {
            child: child.unwrap(),
        })
    }
}

impl<'a> Visitable<'a> for ParenExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        self.child.visit(ctx.clone())
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        self.child.emit(ctx.clone())
    }
}

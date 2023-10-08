use crate::errors::*;
use crate::parser::Parsable;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct BreakStatement {}

impl Parsable for BreakStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Break).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::SemiColon).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        return Some(BreakStatement {});
    }
}

impl<'a> Visitable<'a> for BreakStatement {
    fn visit(
        &self,
        _ctx: Rc<RefCell<NodeContext<'a>>>,
    ) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        todo!();
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let b = ctx.borrow();

        let _ = b.builder.build_unconditional_branch(b.break_pos.unwrap());

        Ok(None)
    }
}

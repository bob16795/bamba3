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
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Break).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::SemiColon).is_none() {
            scn.set_checkpoint(start);
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

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        Ok(false)
    }
}

use crate::nodes::top_expression;
use crate::parser::Parsable;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct BreakStatement {
    pos: FileRange,
}

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

        return Some(BreakStatement {
            pos: (start.1..scn.pos.clone()).into(),
        });
    }
}

impl<'a> Visitable<'a> for BreakStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        todo!();
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        let b = ctx.borrow();
        
        b.builder.build_unconditional_branch(b.break_pos.unwrap());

        Ok(None)
    }
}

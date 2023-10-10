use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct PropExpression {
    pub pos: FileRange,

    pub kind: top_expression::TopExpression,

    pub id: usize,
}

impl Parsable for PropExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Prop).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let kind = top_expression::TopExpression::parse(scn);
        if kind.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        Some(PropExpression {
            pos: (start.1..scn.pos.clone()).into(),
            kind: kind.unwrap(),
            id: 0,
        })
    }
}

impl<'a> Visitable<'a> for PropExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let kind = self.kind.visit(ctx.clone())?;
        kind.borrow_mut().visit()?;

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::Prop {
                id: RefCell::new(self.id),
                kind,
            }),
            ctx,
        })))
    }
}

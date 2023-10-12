use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct DropExpression {
    pub pos: FileRange,

    pub name: String,

    pub id: usize,
}

impl Parsable for DropExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Drop).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let name = scn.match_next(scanner::TokenKind::Identifier);
        if name.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let name_str = name.unwrap().value;

        Some(DropExpression {
            pos: (start.1..scn.pos.clone()).into(),
            name: name_str,
            id: 0,
        })
    }
}

impl<'a> Visitable<'a> for DropExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let b = ctx.borrow();

        let locals = &mut b.locals.borrow_mut();

        if !locals.contains_key(&self.name) {
            return Err(Error::BambaError {
                data: ErrorData::NoLocalError {
                    local: self.name.clone(),
                },
                pos: self.pos.clone(),
            });
        }

        let val = locals.remove(&self.name).unwrap();
        let val: Value = val.1.try_into()?;

        val.emit_drop(self.pos.clone())?;

        return Ok(Rc::new(RefCell::new(Node {
            ctx: ctx.clone(),
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::VoidType),
        })));
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        Ok(self.name == *name)
    }
}

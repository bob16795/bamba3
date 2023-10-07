use crate::parser::*;
use crate::position::*;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct IdentExpression {
    pos: FileRange,

    value: String,
}

impl Parsable for IdentExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let num = scn.match_next(scanner::TokenKind::Identifier);
        if num == None {
            (scn.slice, scn.pos) = start;

            return None;
        }

        Some(IdentExpression {
            pos: (start.1..scn.pos.clone()).into(),
            value: num.unwrap().value,
        })
    }
}

impl<'a> Visitable<'a> for IdentExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        let val = builtin_type(ctx, self.value.clone(), self.pos.clone());

        let val = {
            match val {
                None => {
                    return Err(Error {
                        message: format!("Local value not defined: '{}'", self.value),
                        pos: Some(self.pos.clone()),
                    });
                }
                Some(v) => v,
            }
            .clone()
        };

        let v = &mut val
            .try_borrow_mut()
            .expect(&format!("expected: {}", self.pos));

        v.visit()?;

        Ok(val.clone())
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        let val = builtin_type(ctx, self.value.clone(), self.pos.clone());

        let val = {
            match val {
                None => {
                    return Err(Error {
                        message: format!("Local value not defined: '{}'", self.value),
                        pos: Some(self.pos.clone()),
                    });
                }
                Some(v) => v,
            }
            .clone()
        };

        let res = &mut val.borrow_mut();

        Ok(res.emit()?.clone())
    }
}

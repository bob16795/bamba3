use crate::errors::*;
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
        let start = scn.get_checkpoint();

        let num = scn.match_next(scanner::TokenKind::Identifier);
        if num == None {
            scn.set_checkpoint(start);

            return None;
        }

        Some(IdentExpression {
            pos: (start.1..scn.pos.clone()).into(),
            value: num.unwrap().value,
        })
    }
}

impl<'a> Visitable<'a> for IdentExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let val = builtin_type(ctx, self.value.clone(), self.pos.clone());

        let val = {
            match val {
                None => {
                    return Err(Error::BambaError {
                        data: ErrorData::NoLocalError {
                            local: self.value.clone(),
                        },
                        pos: self.pos.clone(),
                    });
                }
                Some(v) => v,
            }
            .clone()
        };

        let v = &mut val.borrow_mut();

        v.visit()?;

        Ok(val.clone())
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let val = builtin_type(ctx, self.value.clone(), self.pos.clone());

        let val = {
            match val {
                None => {
                    return Err(Error::BambaError {
                        data: ErrorData::NoLocalError {
                            local: self.value.clone(),
                        },
                        pos: self.pos.clone(),
                    });
                }
                Some(v) => v,
            }
            .clone()
        };

        let res = &mut val.borrow_mut();

        Ok(res.emit()?.clone())
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        Ok(self.value == *name)
    }
}

use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pos: FileRange,

    body: Vec<statement::Statement>,
}

impl Parsable for BlockStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::LeftBrace).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let mut body = Vec::new();

        while scn.match_next(scanner::TokenKind::RightBrace).is_none() {
            let def = statement::Statement::parse(scn);
            if def.is_some() {
                body.push(def.unwrap());
            } else {
                if scn.match_next(scanner::TokenKind::RightBrace).is_none() {
                    scn.set_checkpoint(start);
                    return None;
                }

                break;
            }
        }

        Some(BlockStatement {
            pos: (start.1..scn.pos.clone()).into(),

            body,
        })
    }
}

impl<'a> Visitable<'a> for BlockStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let sub_ctx = Rc::new(RefCell::new(ctx.borrow().duplicate()));

        for (_, (id, v)) in &mut sub_ctx.borrow().locals.borrow().iter() {
            let tmp: &mut Option<u32> = &mut id.borrow_mut();

            if tmp.is_some() {
                *tmp = Some(tmp.unwrap() + 1);
            }
        }

        let mut res = Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::VoidType),
            ctx: sub_ctx.clone(),
        }));

        for node in &self.body {
            let node_visited = node.visit(sub_ctx.clone())?;

            match &node_visited.clone().try_into()? {
                Value::VoidType => {}
                _ => {
                    res = node_visited.clone();
                    break;
                }
            }
        }

        let ctxb = sub_ctx.borrow();

        for (_, (id, v)) in &mut ctxb.locals.borrow().iter() {
            let tmp: &mut Option<u32> = &mut id.borrow_mut();

            if tmp.is_some() {
                *tmp = Some(tmp.unwrap() - 1);

                if let Some(returned) = ctxb.returned.clone() {
                    if !*returned.borrow() && tmp.unwrap() == 0 {
                        let v: Value = v.clone().try_into()?;
                        v.emit_drop(self.pos.clone())?;
                    }
                }
            }
        }

        let b = ctx.borrow();

        let loc = &mut b.locals.borrow_mut();

        loc.retain(|_, (id, _)| *id.borrow() != Some(0));

        Ok(res)
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let mut result: Option<Value> = None;

        let sub_ctx = Rc::new(RefCell::new(ctx.borrow().duplicate()));

        for (_, (id, v)) in &mut sub_ctx.borrow().locals.borrow().iter() {
            let tmp: &mut Option<u32> = &mut id.borrow_mut();

            if tmp.is_some() {
                *tmp = Some(tmp.unwrap() + 1);
            }
        }

        for node in &self.body {
            if *sub_ctx.borrow().returned.clone().unwrap().borrow() == true {
                return Err(Error::BambaError {
                    data: ErrorData::CodeAfterReturnError,
                    pos: node.pos.clone(),
                });
            }
            result = node.emit(sub_ctx.clone())?;
        }

        let ctxb = sub_ctx.borrow();

        for (_, (id, v)) in &mut ctxb.locals.borrow().iter() {
            let tmp: &mut Option<u32> = &mut id.borrow_mut();

            if tmp.is_some() {
                *tmp = Some(tmp.unwrap() - 1);

                if let Some(returned) = ctxb.returned.clone() {
                    if !*returned.borrow() && tmp.unwrap() == 0 {
                        let v: Value = v.clone().try_into()?;
                        v.emit_drop(self.pos.clone())?;
                    }
                }
            }
        }

        let b = ctx.borrow();

        let loc = &mut b.locals.borrow_mut();

        loc.retain(|_, (id, _)| *id.borrow() != Some(0));

        Ok(result)
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        for c in &self.body {
            if c.uses(name)? {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

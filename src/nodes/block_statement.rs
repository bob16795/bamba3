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

        let res = Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::VoidType),
            ctx: sub_ctx.clone(),
        }));

        for node in &self.body {
            let node_visited = node.visit(sub_ctx.clone())?;

            match &node_visited.clone().try_into()? {
                Value::VoidType => {}
                _ => return Ok(node_visited.clone()),
            }
        }

        Ok(res)
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let mut result: Option<Value> = None;

        for node in &self.body {
            if *ctx.borrow().returned.clone().unwrap().borrow() == true {
                return Err(Error::BambaError {
                    data: ErrorData::CodeAfterReturnError,
                    pos: node.pos.clone(),
                });
            }
            result = node.emit(ctx.clone())?;
        }

        Ok(result)
    }
}

use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct DollarExpression {
    pos: FileRange,

    vals: Vec<(top_expression::TopExpression, Option<String>)>,
}

impl Parsable for DollarExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Dollar).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::LeftBracket).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let mut vals = Vec::new();

        while scn.match_next(scanner::TokenKind::RightBracket).is_none() {
            let def = top_expression::TopExpression::parse(scn);
            if def.is_some() {
                vals.push((def.unwrap(), None));
                if scn.match_next(scanner::TokenKind::Comma).is_none() {
                    if scn.match_next(scanner::TokenKind::RightBracket).is_none() {
                        scn.set_checkpoint(start);

                        return None;
                    }

                    break;
                }
            } else {
                if scn.match_next(scanner::TokenKind::RightBracket).is_none() {
                    scn.set_checkpoint(start);
                    return None;
                }

                break;
            }
        }

        Some(DollarExpression {
            pos: (start.1..scn.pos.clone()).into(),
            vals,
        })
    }
}

impl<'a> Visitable<'a> for DollarExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let mut children = Vec::new();

        for c in &self.vals {
            children.push(Rc::new(RefCell::new(c.0.visit(ctx.clone())?.try_into()?)));
        }

        Ok(Rc::new(RefCell::new(Node {
            ctx: ctx.clone(),
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::Tuple { children }),
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let mut children = Vec::new();

        for c in &self.vals {
            children.push(Rc::new(RefCell::new(c.0.emit(ctx.clone())?.unwrap())));
        }

        Ok(Some(Value::Tuple { children }))
    }
}

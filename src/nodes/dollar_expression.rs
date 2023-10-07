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

    vals: Vec<top_expression::TopExpression>,
}

impl Parsable for DollarExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        if scn.match_next(scanner::TokenKind::Dollar).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        if scn.match_next(scanner::TokenKind::LeftBrace).is_none() {
            (scn.slice, scn.pos) = start;
            return None;
        }

        let mut vals = Vec::new();

        while scn.match_next(scanner::TokenKind::RightBrace).is_none() {
            let def = top_expression::TopExpression::parse(scn);
            if def.is_some() {
                vals.push(def.unwrap());
                if scn.match_next(scanner::TokenKind::Comma).is_none() {
                    if scn.match_next(scanner::TokenKind::RightBrace).is_none() {
                        (scn.slice, scn.pos) = start;

                        return None;
                    }

                    break;
                }
            } else {
                if scn.match_next(scanner::TokenKind::RightBrace).is_none() {
                    (scn.slice, scn.pos) = start;
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
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        let mut children = Vec::new();

        for c in &self.vals {
            children.push(Rc::new(RefCell::new(c.visit(ctx.clone())?.into())));
        }

        Ok(Rc::new(RefCell::new(Node {
            ctx: ctx.clone(),
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::Tuple { children }),
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        let mut children = Vec::new();

        for c in &self.vals {
            children.push(Rc::new(RefCell::new(c.emit(ctx.clone())?.unwrap())));
        }

        Ok(Some(Value::Tuple { children }))
    }
}
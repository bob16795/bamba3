use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub pos: FileRange,

    pub name: String,
    pub expr: top_expression::TopExpression,
    pub child: statement::Statement,
}

impl Parsable for ForStatement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::For).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let name = scn.match_next(scanner::TokenKind::Identifier);

        if name.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        if scn.match_next(scanner::TokenKind::In).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let expr = top_expression::TopExpression::parse(scn);

        if expr.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let parsed = statement::Statement::parse(scn);
        if parsed.is_some() {
            return Some(ForStatement {
                pos: (start.1..scn.pos.clone()).into(),

                name: name.unwrap().value,
                expr: expr.unwrap(),
                child: parsed.unwrap(),
            });
        }

        scn.set_checkpoint(start);
        None
    }
}

impl<'a> Visitable<'a> for ForStatement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let val = self.expr.visit(ctx.clone())?;
        let Value::Tuple { children } = val.clone().try_into()? else {
            return Err(Error::BambaError {
                data: ErrorData::IterateError {
                    kind: val.try_into()?,
                },
                pos: self.pos.clone(),
            });
        };

        let childv = Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(Value::VoidType),
        }));

        ctx.borrow()
            .locals
            .borrow_mut()
            .insert(self.name.clone(), childv.clone());

        for child in children {
            childv.borrow_mut().value = NodeV::Visited(child.borrow().clone());

            self.child.visit(ctx.clone())?;
        }

        return Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(Value::VoidType),
        })));
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let val = self.expr.visit(ctx.clone())?;
        let Value::Tuple { children } = val.clone().try_into()? else {
            return Err(Error::BambaError {
                data: ErrorData::IterateError {
                    kind: val.try_into()?,
                },
                pos: self.pos.clone(),
            });
        };

        let childv = Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(Value::VoidType),
        }));

        ctx.borrow()
            .locals
            .borrow_mut()
            .insert(self.name.clone(), childv.clone());

        for child in children {
            childv.borrow_mut().value = NodeV::Visited(child.borrow().clone());

            self.child.emit(ctx.clone())?;
        }

        return Ok(Some(Value::VoidType));
    }
}

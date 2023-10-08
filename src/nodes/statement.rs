use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum StatementChild {
    Expression(expression_statement::ExpressionStatement),
    Return(return_statement::ReturnStatement),
    Block(block_statement::BlockStatement),
    While(while_statement::WhileStatement),
    For(for_statement::ForStatement),
    Def(Definition),
    Break(break_statement::BreakStatement),
    If(if_statement::IfStatement),
}

#[derive(Debug, Clone)]
pub struct Statement {
    pos: FileRange,

    child: Box<StatementChild>,
}

impl Parsable for Statement {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let parsed = expression_statement::ExpressionStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::Expression(parsed.unwrap())),
            });
        }

        let parsed = block_statement::BlockStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::Block(parsed.unwrap())),
            });
        }

        let parsed = return_statement::ReturnStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::Return(parsed.unwrap())),
            });
        }

        let parsed = for_statement::ForStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::For(parsed.unwrap())),
            });
        }

        let parsed = while_statement::WhileStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::While(parsed.unwrap())),
            });
        }

        let parsed = if_statement::IfStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::If(parsed.unwrap())),
            });
        }

        let parsed = break_statement::BreakStatement::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::Break(parsed.unwrap())),
            });
        }

        let parsed = Definition::parse(scn);
        if parsed.is_some() {
            return Some(Statement {
                pos: (start.1..scn.pos.clone()).into(),

                child: Box::new(StatementChild::Def(parsed.unwrap())),
            });
        }

        (scn.slice, scn.pos) = start;
        None
    }
}

impl<'a> Visitable<'a> for Statement {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            StatementChild::Expression(ex) => ex.visit(ctx),
            StatementChild::Block(ex) => ex.visit(ctx),
            StatementChild::Return(ex) => ex.visit(ctx),
            StatementChild::Def(ex) => {
                let val = ex.value.visit(ctx.clone())?;

                val.borrow_mut().set_name(ex.name.clone())?;

                let locals = &mut ctx.borrow_mut().locals;
                locals.borrow_mut().insert(ex.name.clone(), val.clone());

                Ok(Rc::new(RefCell::new(Node {
                    pos: self.pos.clone(),
                    ctx: ctx.clone(),
                    value: NodeV::Visited(Value::VoidType),
                })))
            }
            StatementChild::Break(ex) => ex.visit(ctx),
            StatementChild::While(ex) => ex.visit(ctx),
            StatementChild::For(ex) => ex.visit(ctx),
            StatementChild::If(ex) => ex.visit(ctx),
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            StatementChild::Expression(ex) => ex.emit(ctx),
            StatementChild::Block(ex) => ex.emit(ctx),
            StatementChild::Return(ex) => ex.emit(ctx),
            StatementChild::Def(ex) => {
                let val = ex.value.emit(ctx.clone())?;

                let mut res = Node {
                    pos: self.pos.clone(),
                    value: NodeV::Visited(val.unwrap().clone()),
                    ctx: ctx.clone(),
                };

                res.set_name(ex.name.clone())?;

                let locals = &mut ctx.borrow_mut().locals;
                locals
                    .borrow_mut()
                    .insert(ex.name.clone(), Rc::new(RefCell::new(res)));

                Ok(None)
            }
            StatementChild::Break(ex) => ex.emit(ctx),
            StatementChild::While(ex) => ex.emit(ctx),
            StatementChild::For(ex) => ex.emit(ctx),
            StatementChild::If(ex) => ex.emit(ctx),
        }
    }
}

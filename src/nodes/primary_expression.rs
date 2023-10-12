use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum PrimaryExpressionChild {
    ConstInt(ConstIntExpression),
    ConstReal(ConstRealExpression),
    Class(ClassExpression),
    Function(function_expression::FunctionExpression),
    Ident(ident_expression::IdentExpression),
    ConstString(const_string_expression::ConstStringExpression),
    Paren(paren_expression::ParenExpression),
    Include(include_expression::IncludeExpression),
    New(new_expression::NewExpression),
    Dollar(dollar_expression::DollarExpression),
    Comptime(ComptimeExpression),
    Emit(EmitExpression),
    Drop(drop_expression::DropExpression),
}

#[derive(Debug, Clone)]
pub struct PrimaryExpression {
    child: Box<PrimaryExpressionChild>,
}

impl Parsable for PrimaryExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        let parsed = ConstIntExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::ConstInt(parsed.unwrap())),
            });
        }

        let parsed = ConstRealExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::ConstReal(parsed.unwrap())),
            });
        }

        let parsed = ClassExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Class(parsed.unwrap())),
            });
        }

        let parsed = function_expression::FunctionExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Function(parsed.unwrap())),
            });
        }

        let parsed = ident_expression::IdentExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Ident(parsed.unwrap())),
            });
        }

        let parsed = const_string_expression::ConstStringExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::ConstString(parsed.unwrap())),
            });
        }

        let parsed = paren_expression::ParenExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Paren(parsed.unwrap())),
            });
        }

        let parsed = include_expression::IncludeExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Include(parsed.unwrap())),
            });
        }

        let parsed = new_expression::NewExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::New(parsed.unwrap())),
            });
        }

        let parsed = dollar_expression::DollarExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Dollar(parsed.unwrap())),
            });
        }

        let parsed = ComptimeExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Comptime(parsed.unwrap())),
            });
        }

        let parsed = EmitExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Emit(parsed.unwrap())),
            });
        }

        let parsed = drop_expression::DropExpression::parse(scn);
        if parsed.is_some() {
            return Some(PrimaryExpression {
                child: Box::new(PrimaryExpressionChild::Drop(parsed.unwrap())),
            });
        }

        scn.set_checkpoint(start);
        None
    }
}

impl<'a> Visitable<'a> for PrimaryExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match self.child.as_ref() {
            PrimaryExpressionChild::Emit(i) => Ok(Rc::new(RefCell::new(Node {
                value: NodeV::Visited(i.expr.emit(ctx.clone())?.unwrap_or(Value::VoidType)),
                pos: i.pos.clone(),
                ctx,
            }))),
            PrimaryExpressionChild::Comptime(i) => i.expr.visit(ctx.clone()),
            PrimaryExpressionChild::Ident(i) => i.visit(ctx),
            PrimaryExpressionChild::Function(i) => i.visit(ctx),
            PrimaryExpressionChild::ConstString(i) => i.visit(ctx),
            PrimaryExpressionChild::ConstInt(i) => {
                let val = i.value;
                Ok(Rc::new(RefCell::new(Node {
                    value: NodeV::Visited(Value::ConstInt(val)),
                    pos: i.pos.clone(),
                    ctx,
                })))
            }
            PrimaryExpressionChild::ConstReal(i) => {
                let val = i.value;
                Ok(Rc::new(RefCell::new(Node {
                    pos: i.pos.clone(),
                    value: NodeV::Visited(Value::ConstReal(val)),
                    ctx,
                })))
            }
            PrimaryExpressionChild::Drop(i) => i.visit(ctx),
            PrimaryExpressionChild::Include(i) => i.visit(ctx),
            PrimaryExpressionChild::New(i) => i.visit(ctx),
            PrimaryExpressionChild::Dollar(i) => i.visit(ctx),
            PrimaryExpressionChild::Paren(i) => i.visit(ctx),
            PrimaryExpressionChild::Class(cls) => {
                let sub_ctx = Rc::new(RefCell::new(ctx.borrow().duplicate()));

                let children = Rc::new(RefCell::new(HashMap::new()));

                let res = Rc::new(RefCell::new(Node {
                    pos: cls.pos.clone(),
                    value: NodeV::Visited(Value::Class {
                        kind: Rc::new(RefCell::new(None)),
                        children: children.clone(),
                        name: "Anon".to_string(),
                    }),
                    ctx: sub_ctx.clone(),
                }));

                sub_ctx.borrow_mut().self_value = Some(res.clone());

                let mut to_visit = Vec::new();

                let mut prop_idx = 0;

                for def in &cls.defs {
                    let mut cln = def.value.clone();

                    match &mut cln.child.as_mut() {
                        top_expression::TopExpressionChild::Prop(p) => {
                            p.id = prop_idx;

                            prop_idx += 1;

                            to_visit.push(def.name.clone());
                        }
                        _ => {}
                    }

                    let b = Rc::new(RefCell::new(Node {
                        pos: def.pos.clone(),
                        value: NodeV::Unvisited(def.name.clone(), cln),
                        ctx: Rc::new(RefCell::new(sub_ctx.borrow().duplicate())),
                    }));

                    let cb = &mut children.borrow_mut();
                    cb.insert(def.name.clone(), (Rc::new(RefCell::new(None)), b.clone()));

                    sub_ctx
                        .borrow_mut()
                        .locals
                        .borrow_mut()
                        .insert(def.name.clone(), (Rc::new(RefCell::new(None)), b));

                    if def.force {
                        to_visit.push(def.name.clone());
                    }
                }

                for item in to_visit {
                    let val = &mut {
                        let b = &mut sub_ctx.borrow_mut();
                        let b = &mut b.locals.borrow_mut();

                        b.get_mut(&item).unwrap().clone()
                    };

                    val.1.borrow_mut().visit()?;
                }

                Ok(res)
            }
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self.child.as_ref() {
            PrimaryExpressionChild::Drop(i) => i.emit(ctx),
            PrimaryExpressionChild::Ident(i) => i.emit(ctx),
            PrimaryExpressionChild::ConstString(i) => i.emit(ctx),
            PrimaryExpressionChild::Paren(i) => i.emit(ctx),
            PrimaryExpressionChild::ConstInt(i) => Ok(Some(Value::ConstInt(i.value))),
            PrimaryExpressionChild::ConstReal(i) => Ok(Some(Value::ConstReal(i.value))),
            PrimaryExpressionChild::Function(_) => Ok(Some(self.visit(ctx.clone())?.try_into()?)),
            PrimaryExpressionChild::New(i) => i.emit(ctx),
            PrimaryExpressionChild::Comptime(i) => Ok(Some(i.expr.visit(ctx.clone())?.try_into()?)),
            PrimaryExpressionChild::Dollar(i) => i.emit(ctx),
            PrimaryExpressionChild::Class(_) => Ok(Some(self.visit(ctx.clone())?.try_into()?)),
            PrimaryExpressionChild::Emit(i) => i.expr.emit(ctx),
            v => todo!("{:?}", v),
        }
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        match self.child.as_ref() {
            PrimaryExpressionChild::Drop(i) => i.uses(name),
            PrimaryExpressionChild::Ident(i) => i.uses(name),
            PrimaryExpressionChild::ConstString(i) => i.uses(name),
            PrimaryExpressionChild::Paren(i) => i.uses(name),
            PrimaryExpressionChild::ConstInt(_) => Ok(false),
            PrimaryExpressionChild::ConstReal(_) => Ok(false),
            PrimaryExpressionChild::Function(i) => i.uses(name),
            PrimaryExpressionChild::New(i) => i.uses(name),
            PrimaryExpressionChild::Comptime(i) => i.expr.uses(name),
            PrimaryExpressionChild::Dollar(i) => i.uses(name),
            PrimaryExpressionChild::Class(i) => {
                for d in &i.defs {
                    if d.value.uses(name)? {
                        return Ok(true);
                    }
                }

                Ok(false)
            }
            PrimaryExpressionChild::Emit(i) => i.expr.uses(name),
            PrimaryExpressionChild::Include(i) => i.uses(name),
        }
    }
}

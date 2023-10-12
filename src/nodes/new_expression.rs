use crate::errors::*;
use crate::nodes::*;
use crate::parser::*;
use crate::position::*;
use crate::scanner;
use crate::visitable::*;
use inkwell::types::*;
use inkwell::values::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct NewExpression {
    pub pos: FileRange,

    pub kind: unary_expression::UnaryExpression,
}

impl Parsable for NewExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::New).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let kind = unary_expression::UnaryExpression::parse(scn);
        if kind.is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        Some(NewExpression {
            pos: (start.1..scn.pos.clone()).into(),
            kind: kind.unwrap(),
        })
    }
}

impl<'a> Visitable<'a> for NewExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let base_kind = self.kind.visit(ctx.clone())?;
        let bc = ctx.borrow();

        let kind: BasicTypeEnum = match base_kind.clone().borrow_mut().get_type()?.as_ref() {
            AnyTypeEnum::ArrayType(a) => a.clone().into(),
            AnyTypeEnum::FloatType(a) => a.clone().into(),
            AnyTypeEnum::IntType(a) => a.clone().into(),
            AnyTypeEnum::PointerType(a) => a.clone().into(),
            AnyTypeEnum::StructType(a) => a.clone().into(),
            _ => {
                return Err(Error::BambaError {
                    data: ErrorData::VisitUnaryOpError {
                        kind: "new".to_string(),
                        a: base_kind.try_into()?,
                    },
                    pos: self.pos.clone(),
                })
            }
        };

        let val = bc.module.add_global(kind, None, "anonGlobal");

        val.set_initializer(&kind.const_zero());

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(Value::Value {
                val: Rc::new(val.as_pointer_value().into()),
                kind: Rc::new(RefCell::new(Node {
                    pos: self.pos.clone(),
                    ctx: ctx.clone(),
                    value: NodeV::Visited(Value::PointerType(base_kind.clone())),
                })),
                dropable: true,
            }),
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        let base_kind = self.kind.emit(ctx.clone())?.unwrap();

        let base_kind = Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            ctx: ctx.clone(),
            value: NodeV::Visited(base_kind),
        }));

        let kind = base_kind.borrow_mut().get_type()?;

        let ctxb = ctx.borrow();

        let k: BasicTypeEnum = kind.as_ref().clone().try_into().unwrap();

        Ok(Some(Value::Value {
            val: Rc::new(
                ctxb.builder
                    .build_alloca(k, "anonAlloca")
                    .unwrap()
                    .as_basic_value_enum(),
            ),

            kind: Rc::new(RefCell::new(Node {
                ctx: ctx.clone(),

                pos: self.pos.clone(),
                value: NodeV::Visited(Value::PointerType(base_kind)),
            })),
            dropable: true,
        }))
    }

    fn uses(&self, name: &'_ String) -> Result<bool, Error<'a>> {
        self.kind.uses(name)
    }
}

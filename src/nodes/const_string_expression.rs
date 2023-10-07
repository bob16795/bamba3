use crate::parser::*;
use crate::position::*;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ConstStringExpression {
    pos: FileRange,

    value: String,
}

impl Parsable for ConstStringExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = (scn.slice.clone(), scn.pos.clone());

        let num = scn.match_next(scanner::TokenKind::String);
        if num.is_none() {
            (scn.slice, scn.pos) = start;

            return None;
        }

        Some(ConstStringExpression {
            pos: (start.1..scn.pos.clone()).into(),
            value: num.unwrap().value,
        })
    }
}

impl<'a> Visitable<'a> for ConstStringExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error> {
        let val = self.value.to_string();
        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::ConstString(val)),
            ctx,
        })))
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error> {
        let ctxb = ctx.borrow();

        let val = self.value.to_string();
        let string_val = ctxb.context.const_string(&val.clone().into_bytes(), true);
        let string = ctxb
            .module
            .add_global(string_val.get_type(), None, "AnonString");

        string.set_initializer(&string_val);

        Ok(Some(Value::Value {
            val: Rc::new(string.as_pointer_value().into()),
            kind: Rc::new(RefCell::new(Node {
                pos: self.pos.clone(),
                ctx: ctx.clone(),
                value: NodeV::Visited(Value::ArrayType {
                    size: Some((val.len() + 1).try_into().unwrap()),
                    child: Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: ctx.clone(),
                        value: NodeV::Visited(Value::IntType {
                            size: 8,
                            signed: false,
                        }),
                    })),
                }),
            })),
        }))
    }
}

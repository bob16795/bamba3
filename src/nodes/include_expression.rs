use crate::errors::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct IncludeExpression {
    pub pos: FileRange,

    pub file: String,
}

impl Parsable for IncludeExpression {
    fn parse(scn: &mut scanner::Scanner) -> Option<Self> {
        let start = scn.get_checkpoint();

        if scn.match_next(scanner::TokenKind::Import).is_none() {
            scn.set_checkpoint(start);
            return None;
        }

        let file = scn.match_next(scanner::TokenKind::String);
        if file == None {
            scn.set_checkpoint(start);

            return None;
        }

        Some(IncludeExpression {
            pos: (start.1..scn.pos.clone()).into(),
            file: file.unwrap().value,
        })
    }
}

impl<'a> Visitable<'a> for IncludeExpression {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let ctx_borrow = &mut ctx.borrow_mut();

        let file = ctx_borrow.files.get_mut().get_mut(&self.file);

        match file {
            Some(f) => Ok(f.clone()),
            None => {
                let sub_ctx = Rc::new(RefCell::new(ctx_borrow.duplicate()));

                let src = std::fs::read_to_string(self.file.clone()).expect("file not found");

                let mut scn = scanner::Scanner::new(src.trim().to_string(), self.file.clone());

                let file = File::parse(&mut scn);

                match file {
                    Some(file) => {
                        ctx_borrow
                            .files
                            .get_mut()
                            .insert(self.file.clone(), file.visit(sub_ctx)?);

                        Ok(ctx_borrow
                            .files
                            .get_mut()
                            .get_mut(&self.file)
                            .unwrap()
                            .clone())
                    }
                    None => {
                        let range: FileRange = (scn.pos.clone()..scn.far).into();

                        return Err(Error::BambaError {
                            data: ErrorData::ParseError {
                                file: self.file.clone(),
                            },
                            pos: range,
                        });
                    }
                }
            }
        }
    }

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        Ok(Some(self.visit(ctx)?.try_into()?))
    }
}

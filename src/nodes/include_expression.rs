use crate::errors::*;
use crate::parser::*;
use crate::position::FileRange;
use crate::scanner;
use crate::visitable::*;
use std::cell::RefCell;
use std::path::Path;
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

        let in_file = if self.file.clone() == "std" {
            "/usr/lib/bamba/std.bam".to_string()
        } else {
            self.file.clone()
        };

        let path = in_file.clone();
        let path = Path::new(&self.pos.start.file).parent().unwrap().join(path);

        let tmp = &mut ctx_borrow.files.borrow_mut().clone();

        let file = tmp.get_mut(&path);

        match file {
            Some(f) => Ok(f.clone()),
            None => {
                let sub_ctx = Rc::new(RefCell::new(ctx_borrow.duplicate()));

                let src = match std::fs::read_to_string(path.clone()) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Error::BambaError {
                            pos: self.pos.clone(),
                            data: ErrorData::FileError(path.to_str().unwrap().to_string()),
                        })
                    }
                };

                println!("BAM: {}", path.as_path().to_str().unwrap());

                let mut scn = scanner::Scanner::new(
                    src.trim().to_string(),
                    path.to_str().unwrap().to_string().clone(),
                );

                let file = File::parse(&mut scn);

                match file {
                    Some(file) => {
                        ctx_borrow
                            .files
                            .borrow_mut()
                            .insert(path.clone(), file.visit(sub_ctx)?);

                        Ok(ctx_borrow
                            .files
                            .borrow_mut()
                            .get_mut(&path)
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

    fn uses(&self, _: &'_ String) -> Result<bool, Error<'a>> {
        Ok(false)
    }
}

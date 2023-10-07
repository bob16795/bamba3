use crate::parser::Parsable;
use crate::visitable::Visitable;
use clap::Parser;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use bamba3::*;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the file to compile
    filename: String,
}

fn main() -> Result<(), String> {
    let args = Args::parse();

    let src = std::fs::read_to_string(args.filename.clone())
        .expect(&format!("{} not found", args.filename));

    let mut scn = scanner::Scanner::new(src.trim().to_string(), args.filename.to_string());

    let file = parser::File::parse(&mut scn);

    let file = match file {
        Some(file) => file,
        None => {
            let range: position::FileRange = (scn.pos.clone()..scn.far).into();

            return Err(format!("couldnt parse from: {}", range));
        }
    };

    let ctx_ref = Context::create();
    let ctx = Rc::new(&ctx_ref);
    let builder = ctx.create_builder();
    let module = ctx.create_module("sum");
    let files = HashMap::new();
    let externs = HashMap::new();

    let context = visitable::NodeContext {
        locals: Rc::new(RefCell::new(HashMap::new())),

        module: Rc::new(module),
        builder: Rc::new(builder),
        context: ctx.clone(),
        func: None,
        self_value: None,

        files: RefCell::new(files),
        externs: RefCell::new(externs),
        break_pos: None,
    };

    let a = file.visit(Rc::new(RefCell::new(context.clone())));

    let _ = context.module.print_to_file("ir");

    match a {
        Ok(_) => {}
        Err(e) => {
            context.module.print_to_stderr();
            return Err(e.into());
        }
    };

    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Aggressive;
    let reloc = RelocMode::Default;
    let model = CodeModel::Large;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64"),
            "x86-64",
            "",
            opt,
            reloc,
            model,
        )
        .unwrap();

    let _ = target_machine.write_to_file(
        &context.module,
        FileType::Object,
        std::path::Path::new("lol.o"),
    );

    Ok(())
}

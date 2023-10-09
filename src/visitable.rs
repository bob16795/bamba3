extern crate llvm_sys as llvmk;

use crate::errors::*;
use crate::nodes::*;
use crate::parser;
use crate::position::{default_pos, FileRange};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use inkwell::basic_block::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;

pub trait Visitable<'a> {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>>;

    fn emit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        Ok(Some(self.visit(ctx)?.try_into()?))
    }
}

#[derive(Debug, Clone)]
pub struct NodeContext<'ctx> {
    pub locals: Rc<RefCell<HashMap<String, Rc<RefCell<Node<'ctx>>>>>>,
    pub self_value: Option<Rc<RefCell<Node<'ctx>>>>,
    pub context: Rc<&'ctx Context>,
    pub module: Rc<Module<'ctx>>,
    pub builder: Rc<Builder<'ctx>>,
    pub func: Option<FunctionValue<'ctx>>,
    pub files: RefCell<HashMap<String, Rc<RefCell<Node<'ctx>>>>>,

    pub break_pos: Option<BasicBlock<'ctx>>,

    pub externs: RefCell<HashMap<String, FunctionValue<'ctx>>>,

    pub returned: Option<Rc<RefCell<bool>>>,
}

impl<'a> NodeContext<'a> {
    pub fn duplicate(&self) -> Self {
        let mut result = self.clone();

        let mut new_loc = HashMap::new();

        for (name, value) in self.locals.borrow_mut().iter() {
            match value.try_borrow_mut() {
                Ok(value) => {
                    new_loc.insert(name.clone(), Rc::new(RefCell::new(value.clone())));
                }
                _ => {}
            }
        }

        result.locals = Rc::new(RefCell::new(new_loc));

        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionKind {
    Standard,
    Extern,
    //Pointer,
}

#[derive(Debug, Clone)]
pub struct NodeFunctionParam<'a> {
    pub name: String,
    pub val: Option<Rc<RefCell<Node<'a>>>>,

    pub pos: FileRange,
}

#[derive(Debug, Clone)]
pub struct FunctionImpl<'a> {
    pub func: FunctionValue<'a>,
}

#[derive(Debug, Clone)]
pub enum BuiltinFunc {
    AddDef,
    GetProp,
    SetName,
    Print,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    ConstInt(i128),
    ConstReal(f64),
    ConstString(String),
    ConstNull,
    ConstBool(bool),
    Function {
        name: String,

        kind: FunctionKind,
        input: Vec<NodeFunctionParam<'a>>,
        output: Rc<RefCell<Node<'a>>>,

        conts: Option<statement::Statement>,

        impls: Rc<RefCell<HashMap<String, FunctionImpl<'a>>>>,

        ctx: Rc<RefCell<NodeContext<'a>>>,
    },
    Class {
        children: Rc<RefCell<HashMap<String, Rc<RefCell<Node<'a>>>>>>,

        kind: Rc<RefCell<Option<StructType<'a>>>>,

        name: String,
    },

    PointerType(Rc<RefCell<Node<'a>>>),
    ArrayType {
        child: Rc<RefCell<Node<'a>>>,

        size: Option<u32>,
    },
    IntType {
        size: u32,
        signed: bool,
    },
    TypeType,
    ClassType,
    ConstType,
    VoidType,
    FloatType,
    DoubleType,
    BuiltinFunc(BuiltinFunc),
    Tuple {
        children: Vec<Rc<RefCell<Value<'a>>>>,
    },

    Prop {
        id: RefCell<usize>,
        kind: Rc<RefCell<Node<'a>>>,
    },

    Value {
        val: Rc<BasicValueEnum<'a>>,
        kind: Rc<RefCell<Node<'a>>>,
    },
    Method {
        func: Rc<RefCell<Node<'a>>>,
        parent: Rc<RefCell<Node<'a>>>,
    },
}

impl<'a> PartialEq<Value<'a>> for Value<'a> {
    fn eq(&self, other: &Value<'a>) -> bool {
        match (self, other) {
            (Value::Class { .. }, Value::Class { .. }) => true,
            (Value::TypeType, Value::TypeType) => true,
            (Value::PointerType(a), Value::PointerType(b)) => {
                let a: Value = a.clone().try_into().unwrap();

                a == b.clone().try_into().unwrap()
            }
            (
                Value::ArrayType {
                    child: a,
                    size: asize,
                },
                Value::ArrayType {
                    child: b,
                    size: bsize,
                },
            ) => {
                let a: Value = a.clone().try_into().unwrap();

                a == b.clone().try_into().unwrap() && asize == bsize
            }
            (
                Value::IntType {
                    signed: a,
                    size: asize,
                },
                Value::IntType {
                    signed: b,
                    size: bsize,
                },
            ) => a == b && asize == bsize,
            _ => false,
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::ConstInt(val) => write!(f, "Const Int {}", val),
            Value::ConstReal(val) => write!(f, "Const Real {}", val),
            Value::ConstString(val) => write!(f, "Const String '{}'", val),
            Value::ConstNull => write!(f, "Const Null"),
            Value::ConstBool(b) => write!(f, "Const Bool {}", b),
            Value::Function {
                kind: _,
                input: _,
                output: _,
                conts: Some(_),
                impls: _,
                ctx: _,
                name,
            } => write!(f, "Function {}", name),
            Value::Function {
                kind: _,
                input: _,
                output: _,
                conts: None,
                ctx: _,
                impls: _,
                name: _,
            } => write!(f, "Function Type"),
            Value::Class {
                kind: _,
                children,
                name,
            } => write!(
                f,
                "Class named {} with (\n  {}\n)",
                name,
                &children
                    .borrow()
                    .clone()
                    .iter()
                    .fold(String::new(), |acc, num| acc
                        + "\n  "
                        + &format!("{}: {}", &num.0, &num.1.borrow())
                            .to_string()
                            .replace("\n", "\n  "))[3..],
            ),
            Value::Tuple { children } => {
                write!(
                    f,
                    "Tuple length {} of ({})",
                    children.len(),
                    &children.clone().iter().fold(String::new(), |acc, num| acc
                        + ", "
                        + &format!("{}", &num.borrow()).to_string())[2..],
                )
            }

            Value::BuiltinFunc(v) => write!(f, "BuiltinFunction({:?})", v),
            Value::PointerType(v) => write!(f, "Pointer To {}", v.borrow()),
            Value::ArrayType {
                size: Some(size),
                child,
            } => {
                write!(f, "Array Length {} of {}", size, child.borrow())
            }
            Value::ArrayType { size: None, child } => {
                write!(f, "Array of {}", child.borrow())
            }
            Value::IntType { size, signed } => {
                write!(f, "{} bit Integer type signed: {}", size, signed)
            }
            Value::ClassType => write!(f, "Class Type"),
            Value::ConstType => write!(f, "Const Type"),
            Value::TypeType => write!(f, "Type Type"),
            Value::VoidType => write!(f, "Void Type"),
            Value::FloatType => write!(f, "Float Type"),
            Value::DoubleType => write!(f, "Double Type"),
            Value::Value { val: _, kind } => {
                write!(
                    f,
                    "LLVM Value Of Type {}",
                    <Rc<RefCell<Node<'_>>> as TryInto<Value>>::try_into(kind.clone()).unwrap()
                )
            }
            Value::Prop { id, kind } => {
                write!(f, "Property {} of Type {}", id.borrow(), kind.borrow())
            }
            Value::Method { func: _, parent: _ } => write!(f, "Method"),
        }
    }
}

impl<'a> Value<'a> {
    pub fn get_type(
        &mut self,
        ctx: Rc<RefCell<NodeContext<'a>>>,
        pos: FileRange,
    ) -> Result<Box<AnyTypeEnum<'a>>, Error<'a>> {
        match &self {
            Value::Function { output, input, .. } => {
                let out: Option<BasicTypeEnum> =
                    match output.borrow_mut().get_type()?.as_ref().clone() {
                        AnyTypeEnum::PointerType(p) => Some(p.clone().into()),
                        AnyTypeEnum::IntType(p) => Some(p.clone().into()),
                        AnyTypeEnum::StructType(p) => Some(p.clone().into()),
                        AnyTypeEnum::FloatType(p) => Some(p.clone().into()),
                        AnyTypeEnum::ArrayType(p) => Some(p.clone().into()),
                        AnyTypeEnum::VoidType(_) => None,
                        v => todo!("{}", v),
                    };
                let mut inp = Vec::new();

                for i in input {
                    let i: BasicMetadataTypeEnum = match i
                        .val
                        .clone()
                        .unwrap()
                        .borrow_mut()
                        .get_type()?
                        .as_ref()
                        .clone()
                    {
                        AnyTypeEnum::PointerType(p) => p.clone().into(),
                        AnyTypeEnum::IntType(p) => p.clone().into(),
                        AnyTypeEnum::StructType(p) => p.clone().into(),
                        AnyTypeEnum::FloatType(p) => p.clone().into(),
                        AnyTypeEnum::ArrayType(p) => p.clone().into(),
                        v => todo!("{}", v),
                    };
                    inp.push(i);
                }

                match out {
                    Some(out) => Ok(Box::new(out.fn_type(&inp[0..inp.len()], false).into())),
                    None => {
                        let b = &ctx.borrow();

                        Ok(Box::new(
                            b.context
                                .void_type()
                                .fn_type(&inp[0..inp.len()], false)
                                .into(),
                        ))
                    }
                }
            }
            Value::VoidType => {
                let b = &ctx.borrow();

                Ok(Box::new(b.context.void_type().into()))
            }
            Value::DoubleType => {
                let b = &ctx.borrow();

                Ok(Box::new(b.context.f64_type().into()))
            }
            Value::IntType { size, signed: _ } => {
                let b = &ctx.borrow();

                Ok(Box::new(b.context.custom_width_int_type(*size).into()))
            }
            Value::PointerType(child) => {
                let child_type = &mut {
                    let child = child.try_borrow_mut();
                    if child.is_err() {
                        todo!("{:?}", child.err());
                    }

                    child.unwrap().get_type()?
                };
                let b = &ctx.borrow();

                match child_type.as_ref() {
                    AnyTypeEnum::IntType(t) => Ok(Box::new(
                        t.ptr_type(AddressSpace::default()).as_any_type_enum(),
                    )),
                    AnyTypeEnum::VoidType(_) => Ok(Box::new(
                        b.context
                            .custom_width_int_type(0)
                            .ptr_type(AddressSpace::default())
                            .as_any_type_enum(),
                    )),
                    AnyTypeEnum::PointerType(t) => Ok(Box::new(
                        t.ptr_type(AddressSpace::default()).as_any_type_enum(),
                    )),
                    AnyTypeEnum::StructType(t) => Ok(Box::new(
                        t.ptr_type(AddressSpace::default()).as_any_type_enum(),
                    )),
                    AnyTypeEnum::ArrayType(t) => Ok(Box::new(
                        t.ptr_type(AddressSpace::default()).as_any_type_enum(),
                    )),
                    AnyTypeEnum::FunctionType(t) => Ok(Box::new(
                        t.ptr_type(AddressSpace::default()).as_any_type_enum(),
                    )),
                    t => {
                        todo!("impl {}", t)
                    }
                }
            }
            Value::Class {
                kind,
                children,
                name,
            } => {
                if kind.borrow().is_some() {
                    return Ok(Box::new(kind.borrow().unwrap().into()));
                }

                {
                    let b = &ctx.borrow();
                    *kind.borrow_mut() = Some(b.context.opaque_struct_type(name));
                }

                let mut props: Vec<(usize, BasicTypeEnum)> = Vec::new();

                for (_name, child) in children.borrow_mut().iter_mut() {
                    let child = &mut {
                        let child = child.try_borrow_mut();
                        if child.is_err() {
                            continue;
                        }

                        child.unwrap()
                    };

                    let child_v = match &mut child.value {
                        NodeV::Visited(v) => v,
                        NodeV::Emited(v, _) => v,
                        _ => {
                            continue;
                        }
                    };

                    match child_v {
                        Value::Prop {
                            id,
                            kind: base_kind,
                        } => {
                            let kind = base_kind.borrow_mut().get_type()?.as_ref().clone();

                            let id = id.borrow().clone();

                            match kind {
                                AnyTypeEnum::IntType(kind) => {
                                    props.push((id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::FloatType(kind) => {
                                    props.push((id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::ArrayType(kind) => {
                                    props.push((id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::PointerType(kind) => {
                                    props.push((id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::StructType(kind) => {
                                    props.push((id, kind.as_basic_type_enum()))
                                }
                                _ => {
                                    return Err(Error::BambaError {
                                        data: ErrorData::InvalidPropError {
                                            kind: base_kind.clone().try_into()?,
                                        },
                                        pos: pos.clone(),
                                    })
                                }
                            }
                        }
                        _ => {}
                    }
                }

                props.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

                kind.borrow_mut()
                    .unwrap()
                    .set_body(&props.iter().map(|x| x.1).collect::<Vec<_>>(), false);

                Ok(Box::new(kind.borrow_mut().unwrap().into()))
            }
            Value::ArrayType { child, size } => {
                let child_type = &mut child.clone().borrow_mut().get_type()?;

                let size = match size {
                    Some(size) => *size,
                    None => 0,
                };

                match child_type.as_ref() {
                    AnyTypeEnum::IntType(t) => Ok(Box::new(t.array_type(size).as_any_type_enum())),
                    AnyTypeEnum::PointerType(t) => {
                        Ok(Box::new(t.array_type(size).as_any_type_enum()))
                    }
                    AnyTypeEnum::StructType(t) => {
                        Ok(Box::new(t.array_type(size).as_any_type_enum()))
                    }
                    AnyTypeEnum::ArrayType(t) => {
                        Ok(Box::new(t.array_type(size).as_any_type_enum()))
                    }
                    t => {
                        todo!("impl {}", t)
                    }
                }
            }
            _ => Err(Error::BambaError {
                data: ErrorData::ValueTypeError { kind: self.clone() },
                pos: pos.clone(),
            }),
        }
    }

    pub fn get_child(
        &mut self,
        child: String,
        ctx: Rc<RefCell<NodeContext<'a>>>,
        pos: FileRange,
    ) -> Result<Value<'a>, Error<'a>> {
        match child.as_str() {
            "SIZE" => {
                return Ok(Value::Value {
                    val: Rc::new(
                        self.get_type(ctx.clone(), pos.clone())?
                            .size_of()
                            .unwrap()
                            .into(),
                    ),
                    kind: Rc::new(RefCell::new(Node {
                        ctx: ctx.clone(),
                        pos: pos.clone(),
                        value: NodeV::Visited(Value::IntType {
                            size: 64,
                            signed: false,
                        }),
                    })),
                });
            }
            "TYPE" => match self.clone() {
                Value::Value { kind, .. } => {
                    return Ok(kind.clone().try_into()?);
                }
                Value::Class { .. } => {
                    return Ok(Value::ClassType);
                }
                Value::Tuple { .. } => {
                    return Err(Error::BambaError {
                        data: ErrorData::TodoError("tuple type".to_string()),
                        pos: pos.clone(),
                    })
                }
                Value::Method { func, .. } => return Ok(func.try_into()?),
                Value::Function {
                    ctx,
                    impls,
                    input,
                    kind,
                    name,
                    output,
                    ..
                } => {
                    return Ok(Value::Function {
                        conts: None,
                        ctx,
                        impls,
                        input,
                        kind,
                        name,
                        output,
                    });
                }
                Value::Prop { kind, .. } => {
                    return Ok(kind.try_into()?);
                }
                Value::TypeType
                | Value::FloatType
                | Value::ClassType
                | Value::ConstType
                | Value::VoidType
                | Value::DoubleType
                | Value::IntType { .. }
                | Value::ArrayType { .. }
                | Value::PointerType(_) => {
                    return Ok(Value::TypeType);
                }
                Value::ConstNull
                | Value::ConstBool(_)
                | Value::ConstString(_)
                | Value::ConstInt(_)
                | Value::BuiltinFunc(_)
                | Value::ConstReal(_) => {
                    return Ok(Value::ConstType);
                }
            },
            _ => {}
        }

        match &self {
            Value::Class {
                children,
                kind: _,
                name: _,
            } => {
                let children = children.borrow();
                let node = children.get(&child);

                match node.clone() {
                    Some(node) => {
                        let res = node.clone();
                        drop(children);

                        Ok(res.try_into()?)
                    }
                    None => {
                        return Err(Error::BambaError {
                            data: ErrorData::NoChildError {
                                child: child.clone(),
                                parent: self.clone(),
                            },
                            pos: pos.clone(),
                        });
                    }
                }
            }
            Value::Value { val, kind } => {
                kind.borrow_mut().visit()?;

                match kind.clone().try_into()? {
                    Value::PointerType(c) => match c.clone().try_into()? {
                        Value::Class { children, .. } => {
                            let children = {
                                let children = children.borrow();
                                let children = children.get(&child);

                                if children.is_none() {
                                    return Err(Error::BambaError {
                                        data: ErrorData::NoChildError {
                                            child: child.clone(),
                                            parent: self.clone(),
                                        },
                                        pos: pos.clone(),
                                    });
                                }

                                children.unwrap().clone()
                            };

                            if let Value::Function { .. } = children.clone().try_into()? {
                                return Ok(Value::Method {
                                    func: children.clone().into(),
                                    parent: Rc::new(RefCell::new(Node {
                                        pos: pos.clone(),
                                        ctx: ctx.clone(),
                                        value: NodeV::Visited(self.clone()),
                                    })),
                                });
                            };

                            let Value::Prop {
                                id,
                                kind: prop_kind,
                            } = children.clone().try_into()?
                            else {
                                return Err(Error::BambaError {
                                    data: ErrorData::NoChildError {
                                        child: child.clone(),
                                        parent: self.clone(),
                                    },
                                    pos: pos.clone(),
                                });
                            };
                            let bctx = ctx.borrow();

                            Ok(Value::Value {
                                val: Rc::new(
                                    bctx.builder
                                        .build_struct_gep(
                                            c.clone()
                                                .borrow()
                                                .clone()
                                                .get_type()?
                                                .into_struct_type(),
                                            val.into_pointer_value(),
                                            id.clone().borrow().clone() as u32,
                                            &format!("Anon{}", child),
                                        )
                                        .unwrap()
                                        .into(),
                                ),
                                kind: Rc::new(RefCell::new(Node {
                                    pos: pos.clone(),
                                    ctx: ctx.clone(),
                                    value: NodeV::Visited(Value::PointerType(prop_kind)),
                                })),
                            })
                        }
                        _ => Err(Error::BambaError {
                            data: ErrorData::NoChildError {
                                child: child.clone(),
                                parent: self.clone(),
                            },
                            pos: pos.clone(),
                        }),
                    },
                    _ => Err(Error::BambaError {
                        data: ErrorData::NoChildError {
                            child: child.clone(),
                            parent: self.clone(),
                        },
                        pos: pos.clone(),
                    }),
                }
            }
            _ => Err(Error::BambaError {
                data: ErrorData::NoChildError {
                    child: child.clone(),
                    parent: self.clone(),
                },
                pos: pos.clone(),
            }),
        }
    }

    pub fn get_name(&self) -> Result<String, Error<'a>> {
        match self {
            Value::Function {
                kind: _,
                input: _,
                output: _,
                conts: _,
                impls: _,
                name,
                ctx: _,
            } => Ok(name.clone()),
            Value::Value { val: _, kind: _ } => Ok("".to_string()),
            Value::Class {
                kind: _,
                name,
                children: _,
            } => Ok(name.clone()),
            Value::IntType { size, signed: _ } => Ok(format!("i{}", size)),
            _ => Ok("Anon".to_string()),
        }
    }

    pub fn set_name(&mut self, new_name: String) -> Result<(), Error<'a>> {
        match self {
            Value::Function {
                kind: _,
                input: _,
                output: _,
                conts: _,
                impls: _,
                ctx: _,
                name,
            } => {
                *name = new_name;

                Ok(())
            }
            Value::Value { val, kind: _ } => {
                val.set_name(&new_name);

                Ok(())
            }
            Value::Class {
                kind: _,
                name,
                children: _,
            } => {
                *name = new_name;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn emit(&mut self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        match self {
            Value::Function {
                kind,
                input,
                output,
                conts,

                impls,
                name,
                ctx,
            } => {
                if input.len() != 0 && name != "main" {
                    return Ok(Some(self.clone()));
                }

                let mut impl_name = "".to_string();
                let mut args: Vec<BasicMetadataTypeEnum<'a>> = Vec::new();

                for param in input.clone() {
                    match param.val {
                        Some(t) => {
                            let tv = t.clone();
                            let tv = &mut tv.borrow_mut();
                            match (&mut tv.clone()).try_into()? {
                                Value::TypeType => {
                                    impl_name.extend(param.name.chars());
                                }
                                _ => {
                                    let arg = tv.get_type()?;

                                    match arg.as_ref() {
                                        AnyTypeEnum::IntType(ret) => args.push((*ret).into()),
                                        AnyTypeEnum::StructType(ret) => args.push((*ret).into()),
                                        AnyTypeEnum::PointerType(ret) => args.push((*ret).into()),
                                        AnyTypeEnum::FloatType(ret) => args.push((*ret).into()),
                                        AnyTypeEnum::ArrayType(ret) => args.push((*ret).into()),
                                        _ => {
                                            return Err(Error::BambaError {
                                                data: ErrorData::ParamTypeError {
                                                    kind: output.clone().try_into()?,
                                                },
                                                pos: output.borrow().pos.clone(),
                                            })
                                        }
                                    };
                                }
                            }
                        }
                        _ => {
                            impl_name.extend(param.name.chars());
                        }
                    }
                }

                let ret = &mut output.borrow_mut().clone();
                let ret = ret.get_type()?;

                let ty = match &ret.as_ref() {
                    AnyTypeEnum::VoidType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::IntType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::PointerType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::StructType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::ArrayType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::FloatType(ret) => ret.fn_type(&args, false),
                    _ => {
                        return Err(Error::BambaError {
                            data: ErrorData::ReturnTypeError {
                                kind: output.clone().try_into()?,
                            },
                            pos: output.borrow().pos.clone(),
                        })
                    }
                };

                if *kind == FunctionKind::Extern {
                    let ctx_borrowed = ctx.clone();
                    let ctx_borrowed = &ctx_borrowed.borrow();

                    let externs = &mut ctx_borrowed.externs.borrow_mut();

                    match externs.get(name) {
                        Some(func) => {
                            impls
                                .borrow_mut()
                                .insert("".to_string(), FunctionImpl { func: func.clone() });
                        }
                        None => {
                            let func = ctx_borrowed.module.add_function(name, ty, None);

                            func.set_linkage(Linkage::External);

                            impls
                                .borrow_mut()
                                .insert("".to_string(), FunctionImpl { func: func.clone() });

                            externs.insert(name.clone(), func);
                        }
                    }

                    return Ok(Some(self.clone()));
                }

                let returned = Rc::new(RefCell::new(false));

                if conts.is_some() {
                    let (old_func, old, old_ret) = {
                        let ctx_borrowed = &mut ctx.borrow_mut();

                        let old_blk = ctx_borrowed.builder.get_insert_block();
                        let old_func = ctx_borrowed.func;
                        let func = ctx_borrowed.module.add_function(name, ty, None);

                        ctx_borrowed.func = Some(func);

                        let entry = ctx_borrowed.context.append_basic_block(func, "entry");

                        ctx_borrowed.builder.position_at_end(entry);

                        let mut i = 0;

                        for p in func.get_params() {
                            let input = &input[i];

                            ctx_borrowed.locals.borrow_mut().insert(
                                input.name.clone(),
                                Rc::new(RefCell::new(Node {
                                    pos: input.pos.clone(),
                                    ctx: ctx.clone(),
                                    value: NodeV::Visited(Value::Value {
                                        val: p.into(),
                                        kind: input.val.clone().unwrap(),
                                    }),
                                })),
                            );

                            i += 1;
                        }

                        let old_ret = ctx_borrowed.returned.clone();

                        impls
                            .borrow_mut()
                            .insert(impl_name.to_string(), FunctionImpl { func });

                        ctx_borrowed.returned = Some(returned.clone());

                        (old_func, old_blk, old_ret)
                    };

                    conts.clone().unwrap().emit(ctx.clone())?;

                    if old.is_some() {
                        let ctx_borrowed = &ctx.borrow();
                        ctx_borrowed.builder.position_at_end(old.unwrap());
                    } else {
                        let ctx_borrowed = &ctx.borrow();
                        ctx_borrowed.builder.clear_insertion_position();
                    }

                    ctx.borrow_mut().returned = old_ret;
                    ctx.borrow_mut().func = old_func;

                    return Ok(Some(self.clone()));
                }

                // TODO
                return Ok(None);
            }
            Value::ConstString(s) => {
                let ctxb = ctx.borrow();

                let val = s.to_string();
                let string_val = ctxb.context.const_string(&val.clone().into_bytes(), true);
                let string = ctxb
                    .module
                    .add_global(string_val.get_type(), None, "AnonString");

                string.set_initializer(&string_val);

                Ok(Some(Value::Value {
                    val: Rc::new(string.as_pointer_value().into()),
                    kind: Rc::new(RefCell::new(Node {
                        pos: default_pos(),
                        ctx: ctx.clone(),
                        value: NodeV::Visited(Value::ArrayType {
                            size: Some((val.len() + 1).try_into().unwrap()),
                            child: Rc::new(RefCell::new(Node {
                                pos: default_pos(),
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
            _ => Ok(Some(self.clone())),
        }
    }
}

impl<'a> TryFrom<&mut Node<'a>> for Value<'a> {
    type Error = Error<'a>;

    fn try_from(other: &mut Node<'a>) -> Result<Self, Error<'a>> {
        other.visit()?;

        match &other.value {
            NodeV::Visited(v) => Ok(v.clone()),
            NodeV::Emited(v, _) => Ok(v.clone()),
            _ => {
                //let e: String = Error {
                //    message: "".to_string(),
                //    pos: Some(other.pos.clone()),
                //}
                //.into();
                //println!("{}", e);

                todo!("quit");
            }
        }
    }
}

impl<'a> TryFrom<Rc<RefCell<Node<'a>>>> for Value<'a> {
    type Error = Error<'a>;

    fn try_from(other: Rc<RefCell<Node<'a>>>) -> Result<Self, Error> {
        let a: &mut Node<'a> = &mut other.borrow_mut();
        a.try_into()
    }
}

#[derive(Debug, Clone)]
pub enum NodeV<'a> {
    Unvisited(String, top_expression::TopExpression),
    Visited(Value<'a>),
    Emited(Value<'a>, Rc<RefCell<Value<'a>>>),
}

impl fmt::Display for NodeV<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            NodeV::Unvisited(_, _) => {
                write!(f, "Unvisited")
            }
            NodeV::Visited(v) => {
                write!(f, "{}", v)
            }
            NodeV::Emited(a, _v) => {
                write!(f, "{}", a)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
    pub pos: FileRange,
    pub value: NodeV<'a>,

    pub ctx: Rc<RefCell<NodeContext<'a>>>,
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.value)
    }
}

impl<'a> Node<'a> {
    pub fn regen(&mut self, new_name: String) -> Result<(), Error<'a>> {
        match self.try_into()? {
            Value::Class { kind, children, .. } => {
                let mut new_idx = 0;

                for (_, child) in children.borrow_mut().iter_mut() {
                    let child = &mut {
                        let child = child.try_borrow_mut();
                        if child.is_err() {
                            continue;
                        }

                        child.unwrap()
                    };

                    let child_v = match &mut child.value {
                        NodeV::Visited(v) => v,
                        NodeV::Emited(v, _) => v,
                        _ => {
                            continue;
                        }
                    };

                    match child_v {
                        Value::Prop { .. } => new_idx += 1,
                        _ => {}
                    }
                }

                let mut props: Vec<(usize, BasicTypeEnum)> = Vec::new();

                for (name, child) in children.borrow_mut().iter_mut() {
                    let child = &mut {
                        let child = child.try_borrow_mut();
                        if child.is_err() {
                            continue;
                        }

                        child.unwrap()
                    };

                    let child_v = match &mut child.value {
                        NodeV::Visited(v) => v,
                        NodeV::Emited(v, _) => v,
                        _ => {
                            continue;
                        }
                    };

                    match child_v {
                        Value::Prop {
                            id,
                            kind: base_kind,
                        } => {
                            let kind = base_kind.borrow_mut().get_type()?.as_ref().clone();

                            let id: &mut usize = &mut id.borrow_mut();

                            if *name == new_name {
                                *id = new_idx - 1;
                            }

                            match kind {
                                AnyTypeEnum::IntType(kind) => {
                                    props.push((*id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::FloatType(kind) => {
                                    props.push((*id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::ArrayType(kind) => {
                                    props.push((*id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::PointerType(kind) => {
                                    props.push((*id, kind.as_basic_type_enum()))
                                }
                                AnyTypeEnum::StructType(kind) => {
                                    props.push((*id, kind.as_basic_type_enum()))
                                }
                                _ => {
                                    return Err(Error::BambaError {
                                        data: ErrorData::InvalidPropError {
                                            kind: base_kind.clone().try_into()?,
                                        },
                                        pos: self.pos.clone(),
                                    })
                                }
                            }
                        }
                        _ => {}
                    }
                }

                props.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

                if kind.borrow().is_none() {
                    return Ok(());
                }

                kind.borrow_mut()
                    .unwrap()
                    .set_body(&props.iter().map(|x| x.1).collect::<Vec<_>>(), false);

                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn set_name(&mut self, new_name: String) -> Result<(), Error<'a>> {
        match &mut self.value {
            NodeV::Visited(v) => v.set_name(new_name),
            _ => Err(Error::BambaError {
                data: ErrorData::NameUnvisitedError,
                pos: self.pos.clone(),
            }),
        }
    }

    pub fn emit_call(&mut self, params: &mut Vec<Value<'a>>) -> Result<Value<'a>, Error<'a>> {
        match &self.try_into()? {
            Value::Function {
                conts: _,
                kind: _,
                input: _,
                output: _,

                impls: _,
                name: _,
                ctx: _,
            } => {
                let func = self.emit()?;

                let mut p: Vec<BasicMetadataValueEnum> = Vec::new();

                for param in params.clone() {
                    match param {
                        Value::Value { val, kind: _ } => match val.as_ref() {
                            BasicValueEnum::IntValue(arr) => p.push(arr.clone().into()),
                            BasicValueEnum::ArrayValue(arr) => p.push(arr.clone().into()),
                            BasicValueEnum::PointerValue(arr) => p.push(arr.clone().into()),
                            BasicValueEnum::StructValue(arr) => p.push(arr.clone().into()),
                            BasicValueEnum::FloatValue(arr) => p.push(arr.clone().into()),
                            _ => {
                                return Err(Error::BambaError {
                                    data: ErrorData::NoValueError,
                                    pos: self.pos.clone(),
                                });
                            }
                        },
                        _ => {}
                    }
                }

                match func {
                    Some(Value::Function {
                        output: out, ctx, ..
                    }) => {
                        let func_impl = self.clone().get_impl(Some(params))?;

                        let bctx = &mut ctx.borrow_mut();

                        let val = bctx.builder.build_call(func_impl.func, &p, "call");

                        val.as_ref().unwrap().set_call_convention(0);

                        let val = val.unwrap().try_as_basic_value().left();

                        match val {
                            Some(val) => Ok(Value::Value {
                                val: Rc::new(val.into()),
                                kind: out.clone(),
                            }),
                            None => Ok(Value::VoidType),
                        }
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::NoValueError,
                        pos: self.pos.clone(),
                    }),
                }
            }
            Value::Class { .. } => match params[0].emit(self.ctx.clone())? {
                Some(Value::Tuple { children }) => {
                    let mut vals = Vec::new();

                    // TODO: check assignable

                    for c in children {
                        let Value::Value { val: c, kind: _ } = c.borrow().clone() else {
                            return Err(Error::BambaError {
                                data: ErrorData::NoValueError,
                                pos: self.pos.clone(),
                            });
                        };

                        vals.push(c.as_ref().clone());
                    }

                    let br = self.ctx.borrow();

                    let val = br.context.const_struct(&vals, false);

                    Ok(Value::Value {
                        val: val.as_basic_value_enum().into(),
                        kind: Rc::new(RefCell::new(self.clone())),
                    })
                }
                None => {
                    return Err(Error::BambaError {
                        data: ErrorData::NoValueError,
                        pos: self.pos.clone(),
                    });
                }
                _ => Err(Error::BambaError {
                    data: ErrorData::CastError {
                        value: params[0].clone(),
                        kind: self.try_into()?,
                    },
                    pos: self.pos.clone(),
                }),
            },
            Value::Method { func, parent } => {
                let mut new_params: Vec<Value> = vec![parent.clone().try_into()?];
                new_params.extend(params.clone());

                return func.borrow_mut().emit_call(&mut new_params);
            }

            Value::DoubleType => match params[0].emit(self.ctx.clone())? {
                Some(Value::ConstInt(r)) => Ok(Value::Value {
                    val: Rc::new(
                        self.ctx
                            .borrow()
                            .context
                            .f64_type()
                            .const_float(r as f64)
                            .into(),
                    ),
                    kind: Rc::new(RefCell::new(self.clone())),
                }),
                Some(Value::ConstReal(r)) => Ok(Value::Value {
                    val: Rc::new(self.ctx.borrow().context.f64_type().const_float(r).into()),
                    kind: Rc::new(RefCell::new(self.clone())),
                }),
                Some(Value::Value { val, .. }) => {
                    let ctxb = self.ctx.clone();
                    let ctxb = ctxb.borrow();
                    match val.as_ref() {
                        BasicValueEnum::IntValue(i) => Ok(Value::Value {
                            val: Rc::new(
                                ctxb.builder
                                    .build_unsigned_int_to_float(
                                        *i,
                                        ctxb.context.f64_type(),
                                        "floatCast",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum(),
                            ),
                            kind: Rc::new(RefCell::new(self.clone())),
                        }),
                        BasicValueEnum::FloatValue(i) => Ok(Value::Value {
                            val: Rc::new(
                                ctxb.builder
                                    .build_float_cast(*i, ctxb.context.f64_type(), "floatCast")
                                    .unwrap()
                                    .as_basic_value_enum(),
                            ),
                            kind: Rc::new(RefCell::new(self.clone())),
                        }),
                        _ => {
                            let val: Value = self.try_into()?;
                            Err(Error::BambaError {
                                data: ErrorData::CastError {
                                    value: params[0].clone(),
                                    kind: val.clone(),
                                },
                                pos: self.pos.clone(),
                            })
                        }
                    }
                }
                _ => Err(Error::BambaError {
                    data: ErrorData::CastError {
                        value: params[0].clone(),
                        kind: self.try_into()?,
                    },
                    pos: self.pos.clone(),
                }),
            },
            Value::IntType { size: _, signed } => {
                let val = params[0].emit(self.ctx.clone())?;

                match val {
                    Some(Value::ConstBool(b)) => {
                        let signed = signed.clone();

                        let out = self
                            .get_type()?
                            .as_ref()
                            .into_int_type()
                            .const_int(b as u64, signed);

                        Ok(Value::Value {
                            val: Rc::new(out.into()),
                            kind: Rc::new(RefCell::new(self.clone())),
                        })
                    }
                    Some(Value::ConstInt(v)) => {
                        let signed = signed.clone();

                        let out = self
                            .get_type()?
                            .as_ref()
                            .into_int_type()
                            .const_int(v as u64, signed);

                        Ok(Value::Value {
                            val: Rc::new(out.into()),
                            kind: Rc::new(RefCell::new(self.clone())),
                        })
                    }
                    Some(Value::Value { val, kind: _ }) => {
                        let out: IntValue = match val.as_ref() {
                            BasicValueEnum::IntValue(v) => {
                                let cl = self.clone();
                                let b = cl.ctx.borrow();

                                b.builder
                                    .build_int_cast(
                                        (*v).into(),
                                        self.get_type()?.as_ref().into_int_type(),
                                        "int cast",
                                    )
                                    .unwrap()
                            }
                            BasicValueEnum::PointerValue(v) => {
                                let cl = self.clone();
                                let b = cl.ctx.borrow();

                                b.builder
                                    .build_ptr_to_int(
                                        *v,
                                        self.get_type()?.as_ref().into_int_type(),
                                        "intCast",
                                    )
                                    .unwrap()
                            }
                            BasicValueEnum::FloatValue(v) => {
                                let cl = self.clone();
                                let b = cl.ctx.borrow();

                                b.builder
                                    .build_float_to_unsigned_int(
                                        *v,
                                        self.get_type()?.as_ref().into_int_type(),
                                        "intCast",
                                    )
                                    .unwrap()
                            }

                            _ => {
                                return Err(Error::BambaError {
                                    data: ErrorData::CastError {
                                        value: params[0].clone(),
                                        kind: self.try_into()?,
                                    },
                                    pos: self.pos.clone(),
                                });
                            }
                        };
                        Ok(Value::Value {
                            val: Rc::new(out.into()),
                            kind: Rc::new(RefCell::new(self.clone())),
                        })
                    }

                    _ => Err(Error::BambaError {
                        data: ErrorData::CastError {
                            value: params[0].clone(),
                            kind: self.try_into()?,
                        },
                        pos: self.pos.clone(),
                    }),
                }
            }
            Value::PointerType(pkind) => match &params[0] {
                Value::Value { kind: _, val } => {
                    pkind.borrow_mut().visit()?;

                    match val.as_ref() {
                        BasicValueEnum::PointerValue(_ptr) => {
                            self.get_type()?;

                            Ok(Value::Value {
                                val: val.clone(),
                                kind: Rc::new(RefCell::new(self.clone())),
                            })
                        }
                        BasicValueEnum::IntValue(int) => {
                            let kind = self.get_type()?;

                            let val = self
                                .ctx
                                .borrow()
                                .builder
                                .build_int_to_ptr(int.clone(), kind.into_pointer_type(), "inttoptr")
                                .unwrap()
                                .as_basic_value_enum();

                            Ok(Value::Value {
                                val: Rc::new(val.clone()),
                                kind: Rc::new(RefCell::new(self.clone())),
                            })
                        }
                        _ => Err(Error::BambaError {
                            data: ErrorData::CastError {
                                value: params[0].clone(),
                                kind: self.try_into()?,
                            },
                            pos: self.pos.clone(),
                        }),
                    }
                }
                Value::ConstNull => {
                    pkind.borrow_mut().visit()?;

                    let val = self
                        .get_type()?
                        .into_pointer_type()
                        .const_zero()
                        .as_basic_value_enum();

                    Ok(Value::Value {
                        val: Rc::new(val.clone()),
                        kind: Rc::new(RefCell::new(self.clone())),
                    })
                }
                _ => Err(Error::BambaError {
                    data: ErrorData::NoNullError {
                        kind: self.try_into()?,
                    },
                    pos: self.pos.clone(),
                }),
            },

            Value::Value { val, kind } => {
                let cl = self.clone();
                let b = cl.ctx.borrow();

                let Value::PointerType(kind) = kind.clone().try_into()? else {
                    return Err(Error::BambaError {
                        data: ErrorData::PtrCallError {
                            value: kind.clone().try_into()?,
                        },
                        pos: self.pos.clone(),
                    });
                };

                let mut new = Vec::new();

                for p in params {
                    let p: Value = p.clone().into();
                    let Value::Value { val: p, kind: _ } = p else {
                        return Err(Error::BambaError {
                            data: ErrorData::PtrCallError {
                                value: kind.try_into()?,
                            },
                            pos: self.pos.clone(),
                        });
                    };
                    new.push(p.as_ref().clone().into())
                }

                let result = b.builder.build_indirect_call(
                    kind.clone()
                        .borrow_mut()
                        .get_type()?
                        .as_ref()
                        .into_function_type(),
                    val.into_pointer_value(),
                    &new,
                    "iCall",
                );

                let result = result.as_ref().unwrap();

                let Value::Function { output, .. }: Value = kind.clone().try_into()? else {
                    return Err(Error::BambaError {
                        data: ErrorData::PtrCallError {
                            value: kind.try_into()?,
                        },
                        pos: self.pos.clone(),
                    });
                };

                match result.try_as_basic_value().left() {
                    Some(v) => Ok(Value::Value {
                        val: Rc::new(v),
                        kind: output,
                    }),
                    None => Ok(Value::VoidType),
                }
            }

            _ => Err(Error::BambaError {
                data: ErrorData::CastError {
                    value: params[0].clone(),
                    kind: self.try_into()?,
                },
                pos: self.pos.clone(),
            }),
        }
    }

    pub fn get_type(&mut self) -> Result<Box<AnyTypeEnum<'a>>, Error<'a>> {
        self.visit()?;

        let v: &mut Value = &mut self.try_into()?;

        v.get_type(self.ctx.clone(), self.pos.clone())
    }

    pub fn get_child(&mut self, child: String) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        self.visit()?;

        let v: &mut Value = &mut self.try_into()?;

        Ok(Rc::new(RefCell::new(Node {
            ctx: self.ctx.clone(),
            pos: self.pos.clone(),
            value: NodeV::Visited(v.get_child(child, self.ctx.clone(), self.pos.clone())?),
        })))
    }

    pub fn call(
        &mut self,
        params: Vec<Rc<RefCell<Node<'a>>>>,
    ) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        match &self.try_into()? {
            Value::Function {
                conts,
                kind: _,
                input,
                output: _,
                impls: _,
                name: _,
                ctx,
            } => {
                let sub_ctx = Rc::new(RefCell::new(ctx.borrow().duplicate()));

                let mut idx = 0;
                for i in input {
                    sub_ctx
                        .borrow_mut()
                        .locals
                        .borrow_mut()
                        .insert(i.name.clone(), params[idx].clone());
                    idx += 1;
                }

                let res = conts.clone().unwrap().visit(sub_ctx)?;
                let res = res.borrow().clone();

                Ok(Rc::new(RefCell::new(res)))
            }

            _ => {
                let mut new = Vec::new();

                for p in params {
                    new.push(p.try_into()?);
                }

                let v = self.emit_call(&mut new)?;

                Ok(Rc::new(RefCell::new(Node {
                    pos: self.pos.clone(),
                    ctx: self.ctx.clone(),
                    value: NodeV::Visited(v),
                })))
            }
        }
    }

    pub fn get_impl(
        &mut self,
        params: Option<&[Value<'a>]>,
    ) -> Result<FunctionImpl<'a>, Error<'a>> {
        let val: &mut Value = &mut self.try_into()?;
        let mut name = format!("");
        let mut args: Vec<BasicMetadataTypeEnum<'a>> = Vec::new();

        let Value::Function {
            impls,
            input,
            output,
            kind,
            conts,
            name: func_name,
            ctx,
        } = val
        else {
            todo!()
        };

        if *kind == FunctionKind::Extern {
            return Ok(impls.borrow().get("").unwrap().clone());
        }

        assert!(
            params.is_none() || input.len() == params.unwrap().len(),
            "{}, {}, {}",
            self.pos,
            input.len(),
            params.unwrap().len(),
        );

        let sub_ctx = Rc::new(RefCell::new(ctx.borrow().duplicate()));

        for (idx, input) in input.iter().enumerate() {
            let p = match params {
                Some(params) => Some(&params[idx]),
                None => None,
            };

            let mut add: bool = false;

            match p {
                Some(Value::Value { .. }) | None => {
                    let val = input.val.clone();
                    match val {
                        Some(val) => {
                            let v: Value = val.clone().try_into()?;
                            if v == Value::TypeType {
                                add = true;
                            } else {
                                let val = val.borrow_mut().get_type()?;

                                match val.as_ref() {
                                    AnyTypeEnum::IntType(ret) => args.push(ret.clone().into()),
                                    AnyTypeEnum::StructType(ret) => args.push((*ret).into()),
                                    AnyTypeEnum::PointerType(ret) => args.push((*ret).into()),
                                    AnyTypeEnum::ArrayType(ret) => args.push((*ret).into()),
                                    AnyTypeEnum::FloatType(ret) => args.push((*ret).into()),
                                    _ => todo!(),
                                };
                            }
                        }
                        None => add = true,
                    };
                }
                _ => {
                    add = true;
                }
            }

            if add {
                sub_ctx.borrow().locals.borrow_mut().insert(
                    input.name.clone(),
                    Rc::new(RefCell::new(Node {
                        pos: self.pos.clone(),
                        ctx: sub_ctx.clone(),

                        value: NodeV::Visited(p.unwrap().clone()),
                    })),
                );
                name.extend(p.unwrap().get_name());
            }
        }

        let i = impls.borrow().clone();
        let implementation = i.get(&name);

        match implementation {
            Some(v) => Ok(v.clone()),
            None => {
                let kind = &mut output.borrow_mut().clone();
                let ret = kind.get_type()?;

                let ty = match &ret.as_ref() {
                    AnyTypeEnum::VoidType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::IntType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::PointerType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::StructType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::ArrayType(ret) => ret.fn_type(&args, false),
                    AnyTypeEnum::FloatType(ret) => ret.fn_type(&args, false),
                    _ => {
                        return Err(Error::BambaError {
                            data: ErrorData::ReturnTypeError {
                                kind: kind.try_into()?,
                            },
                            pos: output.borrow().pos.clone(),
                        })
                    }
                };

                let returned = Rc::new(RefCell::new(false));

                let (old_func, old, old_returned) = {
                    let ctx_borrowed = &mut sub_ctx.borrow_mut();

                    let old_blk = ctx_borrowed.builder.get_insert_block();
                    let func = ctx_borrowed.module.add_function(
                        &format!("{}({})", func_name, name),
                        ty,
                        None,
                    );

                    let old_func = ctx_borrowed.func;

                    ctx_borrowed.func = Some(func);

                    let entry = ctx_borrowed.context.append_basic_block(func, "entry");

                    ctx_borrowed.builder.position_at_end(entry);

                    let mut i = 0;

                    for (idx, input) in input.iter().enumerate() {
                        let p = match params {
                            Some(params) => Some(&params[idx]),
                            None => None,
                        };

                        let v: Value = input.val.clone().unwrap().try_into()?;
                        if v != Value::TypeType && input.val.clone().is_some() {
                            match p {
                                Some(Value::Value { .. }) | None => {
                                    ctx_borrowed.locals.borrow_mut().insert(
                                        input.name.clone(),
                                        Rc::new(RefCell::new(Node {
                                            pos: input.pos.clone(),
                                            ctx: sub_ctx.clone(),
                                            value: NodeV::Visited(Value::Value {
                                                val: func.get_params()[i].into(),
                                                kind: input.val.clone().unwrap(),
                                            }),
                                        })),
                                    );

                                    i += 1;
                                }
                                _ => {}
                            }
                        } else {
                            ctx_borrowed.locals.borrow_mut().insert(
                                input.name.clone(),
                                Rc::new(RefCell::new(Node {
                                    pos: input.pos.clone(),
                                    ctx: sub_ctx.clone(),
                                    value: NodeV::Visited(p.unwrap().clone()),
                                })),
                            );
                        }
                    }

                    impls
                        .borrow_mut()
                        .insert(name.to_string(), FunctionImpl { func });

                    let old_returned = ctx_borrowed.returned.clone();

                    ctx_borrowed.returned = Some(returned.clone());

                    (old_func, old_blk, old_returned)
                };

                conts.clone().unwrap().emit(sub_ctx.clone())?;

                let ctx_borrowed = &mut sub_ctx.borrow_mut();

                if old.is_some() {
                    ctx_borrowed.builder.position_at_end(old.unwrap());
                } else {
                    ctx_borrowed.builder.clear_insertion_position();
                }

                ctx_borrowed.func = old_func;
                ctx_borrowed.returned = old_returned;

                return Ok(impls.borrow().get(&name).unwrap().clone());
            }
        }
    }
}

impl<'a> Node<'a> {
    pub fn emit(&mut self) -> Result<Option<Value<'a>>, Error<'a>> {
        match &mut self.value {
            NodeV::Unvisited(_name, _e) => {
                self.visit()?;

                self.emit()
            }
            NodeV::Visited(c) => {
                let res = c.emit(self.ctx.clone())?;
                if res.is_some() {
                    self.value = NodeV::Emited(
                        c.clone(),
                        Rc::new(RefCell::new(res.clone().unwrap().clone())),
                    );
                }

                Ok(res)
            }
            NodeV::Emited(_, e) => Ok(Some(e.borrow().clone())),
        }
    }

    pub fn visit(&mut self) -> Result<(), Error<'a>> {
        match &self.value.clone() {
            NodeV::Unvisited(name, e) => {
                let res = e.visit(self.ctx.clone())?;
                *self = res.borrow().clone();

                self.set_name(name.clone())?;

                Ok(())
            }
            NodeV::Visited(_) => Ok(()),
            NodeV::Emited(_, _) => Ok(()),
        }
    }
}

pub fn builtin_type<'a>(
    ctx: Rc<RefCell<NodeContext<'a>>>,
    name: String,
    pos: FileRange,
) -> Option<Rc<RefCell<Node<'a>>>> {
    match name.as_str() {
        "Void" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::VoidType),
                ctx,
            })));
        }
        "Self" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(
                    ctx.clone()
                        .borrow()
                        .self_value
                        .clone()
                        .unwrap()
                        .try_into()
                        .unwrap(),
                ),
                ctx,
            })));
        }
        "usize" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::IntType {
                    size: 64,
                    signed: false,
                }),
                ctx,
            })));
        }
        "Bool" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::IntType {
                    size: 1,
                    signed: false,
                }),
                ctx,
            })));
        }
        "true" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::ConstBool(true)),
                ctx,
            })));
        }
        "false" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::ConstBool(false)),
                ctx,
            })));
        }
        "null" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::ConstNull),
                ctx,
            })));
        }
        "Type" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::TypeType),
                ctx,
            })));
        }
        "Class" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::ClassType),
                ctx,
            })));
        }
        "f32" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::FloatType),
                ctx,
            })));
        }
        "f64" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::DoubleType),
                ctx,
            })));
        }

        "@PRINT" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::BuiltinFunc(BuiltinFunc::Print)),
                ctx,
            })));
        }
        "@SET_NAME" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::BuiltinFunc(BuiltinFunc::SetName)),
                ctx,
            })));
        }
        "@ADD_DEF" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::BuiltinFunc(BuiltinFunc::AddDef)),
                ctx,
            })));
        }
        "@GET_PROP" => {
            return Some(Rc::new(RefCell::new(Node {
                pos: pos.clone(),
                value: NodeV::Visited(Value::BuiltinFunc(BuiltinFunc::GetProp)),
                ctx,
            })));
        }
        &_ => {}
    }

    match &name.chars().nth(0) {
        Some('i') => {
            let size = name[1..].parse();
            if size.is_ok() {
                return Some(Rc::new(RefCell::new(Node {
                    pos: pos.clone(),
                    value: NodeV::Visited(Value::IntType {
                        signed: true,
                        size: size.unwrap(),
                    }),
                    ctx,
                })));
            }
        }
        Some('u') => {
            let size = name[1..].parse();
            if size.is_ok() {
                return Some(Rc::new(RefCell::new(Node {
                    pos: pos.clone(),
                    value: NodeV::Visited(Value::IntType {
                        signed: false,
                        size: size.unwrap(),
                    }),
                    ctx,
                })));
            }
        }
        _ => {}
    }
    let b = &mut ctx.borrow_mut();
    let val = b.locals.borrow();
    let val = val.get(&name);

    val.cloned()
}

impl<'a> Visitable<'a> for parser::File {
    fn visit(&self, ctx: Rc<RefCell<NodeContext<'a>>>) -> Result<Rc<RefCell<Node<'a>>>, Error<'a>> {
        let mut to_visit = Vec::new();

        for def in &self.defs {
            let b = Rc::new(RefCell::new(Node {
                pos: def.pos.clone(),
                value: NodeV::Unvisited(def.name.clone(), def.value.clone()),
                ctx: ctx.clone(),
            }));

            ctx.borrow_mut()
                .locals
                .borrow_mut()
                .insert(def.name.clone(), b);

            if def.force {
                to_visit.push(def.name.clone());
            }
        }

        for item in to_visit {
            let val = &mut {
                let b = &mut ctx.borrow_mut();

                let b = &mut b.locals.borrow_mut();

                b.get_mut(&item).unwrap().clone()
            };

            let b = &mut val.borrow_mut();

            b.emit()?;
        }

        Ok(Rc::new(RefCell::new(Node {
            pos: self.pos.clone(),
            value: NodeV::Visited(Value::Class {
                kind: Rc::new(RefCell::new(None)),
                children: ctx.borrow().locals.clone(),
                name: "Anon".to_string(),
            }),
            ctx: ctx.clone(),
        })))
    }

    fn emit(&self, _: Rc<RefCell<NodeContext<'a>>>) -> Result<Option<Value<'a>>, Error<'a>> {
        todo!("emit file");
    }
}

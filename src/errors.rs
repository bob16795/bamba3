use crate::position::FileRange;
use crate::visitable::Value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ErrorData<'a> {
    #[error("Local not defined '{local}'")]
    NoLocalError { local: String },
    #[error("Child not found '{child}' in {parent}")]
    NoChildError { child: String, parent: Value<'a> },
    #[error("Cant check if value '{value}' is zero")]
    ZeroCompareError { value: Value<'a> },
    #[error("Expected value")]
    NoValueError,
    #[error("Cant create pointer to type '{kind}'")]
    PointerTypeError { kind: Value<'a> },
    #[error("Cant deref value of type '{kind}'")]
    DerefValueError { kind: Value<'a> },
    #[error("Cant visit operation {kind} on param '{a}'")]
    VisitUnaryOpError { kind: String, a: Value<'a> },
    #[error("Cant visit operation {kind} on params '{a}', '{b}'")]
    VisitBinaryOpError {
        kind: String,
        a: Value<'a>,
        b: Value<'a>,
    },
    #[error("Cant emit operation {kind} on param '{a}'")]
    EmitUnaryOpError { kind: String, a: Value<'a> },
    #[error("Cant emit operation {kind} on params '{a}', '{b}'")]
    EmitBinaryOpError {
        kind: String,
        a: Value<'a>,
        b: Value<'a>,
    },
    #[error("Cant emit null for type {kind}")]
    NoNullError { kind: Value<'a> },
    #[error("Cant emit return for type {kind}")]
    NoEmitReturnTypeError { kind: Value<'a> },
    #[error("Cant parse file {file}")]
    ParseError { file: String },
    #[error("Cant iterate through type {kind}")]
    IterateError { kind: Value<'a> },
    #[error("Cant read type as string {kind}")]
    StringError { kind: Value<'a> },
    #[error("Expected class type found {kind}")]
    ClassError { kind: Value<'a> },
    #[error("Cant return value {kind}")]
    ReturnTypeError { kind: Value<'a> },
    #[error("Cant create param value {kind}")]
    ParamTypeError { kind: Value<'a> },
    #[error("Cant get type of value {kind}")]
    ValueTypeError { kind: Value<'a> },
    #[error("Cant cast value {value} to {kind}")]
    CastError { value: Value<'a>, kind: Value<'a> },
    #[error("Cant call non function pointer {value}")]
    PtrCallError { value: Value<'a> },
    #[error("Cant set name of unvisited node")]
    NameUnvisitedError,
    #[error("Cant create prop of type {kind}")]
    InvalidPropError { kind: Value<'a> },
    #[error("Code after return statement")]
    CodeAfterReturnError,
    #[error("{value}")]
    ErrorFunctionCall { value: String },

    #[error("{0} not yet implemented")]
    TodoError(String),
}

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error("Error: {data}\n\n{pos}\n")]
    BambaError { pos: FileRange, data: ErrorData<'a> },
    #[error(transparent)]
    Other(#[from] inkwell::builder::BuilderError),
}

use crate::string::Str;

#[derive(Debug, Clone)]
pub enum Definition {
    Function(Function),
    Method {
        receiver: Expression,
        function: Function,
    },
    Variable {
        name: Str,
        mutable: bool,
        expr: Expression,
    },
    Struct {
        name: Str,
        fields: Vec<(Str, bool)>,
    },
    Module(Str),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Str,
    pub args: Vec<Str>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Let {
        name: Str,
        mutable: bool,
        expr: Expression,
    },
    Assign {
        name: Str,
        expr: Expression,
        op: Option<Str>,
    },
    FieldAssign {
        dict: Expression,
        field: Expression,
        expr: Expression,
        op: Option<Str>,
    },
    Defer {
        expr: Expression,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Op {
        name: Str,
        args: Vec<Expression>,
    },
    Call {
        callee: Box<Expression>,
        args: Vec<VecAppend>,
    },
    Literal {
        value: Literal,
    },
    Vec {
        appends: Vec<VecAppend>,
    },
    Dict {
        appends: Vec<DictAppend>,
    },
    Struct {
        constructor: Box<Expression>,
        appends: Vec<DictAppend>,
    },
    Variable {
        name: Str,
    },
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        else_: Option<Box<Expression>>,
    },
    // NOTE: Loop is always wrapped by Labeled
    Loop {
        body: Box<Expression>,
    },
    // Labeled is only breakable (and continueable) block
    Labeled {
        label: Str,
        body: Box<Expression>,
    },
    Match {
        expr: Box<Expression>,
        arms: Vec<(Pattern, Expression)>,
    },
    Block(Block),
    Return {
        expr: Option<Box<Expression>>,
    },
    Break {
        label: Str,
        expr: Option<Box<Expression>>,
    },
    Continue {
        label: Str,
    },
    Closure(Box<Function>),
    // Unused so far
    Env(Box<Expression>),
    StaticNativeFn {
        native_fn: &'static crate::compile::NativeFn,
    },
}

#[derive(Debug, Clone)]
pub enum VecAppend {
    Element(Expression),
    Spread(Expression),
}

#[derive(Debug, Clone)]
pub enum DictAppend {
    Field(Str, Expression),
    Spread(Expression),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Variable {
        name: Str,
        type_: Option<Box<Pattern>>,
    },
    Literal(Literal),
    Vec {
        values: Vec<Pattern>,
        allow_tail: bool,
    },
    Dict(Vec<(Str, Pattern)>),
    Struct {
        constructor: Str,
        fields: Vec<(Str, Pattern)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    Null,
    Int(i64),
    Float(f64),
    String(Str),
    Bool(bool),
}

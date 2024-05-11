mod lexer;
pub mod macros;
mod string;

use std::sync::Arc;

use nom::{
    branch::alt,
    combinator::{cut, fail, map, not, opt, peek, value},
    multi::{many0, separated_list0},
    sequence::{preceded, terminated},
    IResult,
};

use crate::{
    model::{
        Block, Definition, DictAppend, Expression, Function, Literal, Pattern, Statement, VecAppend,
    },
    string::{intern, Str},
};

use self::{
    lexer::{lex, AsStr, Token},
    macros::{
        parser::{macro_call, macro_def},
        Macro,
    },
};

thread_local! {
    static MACROS: std::cell::RefCell<Vec<Arc<Macro>>> = std::cell::RefCell::new(Vec::new());
}

pub fn parse(src: &str) -> Result<Vec<Definition>, String> {
    MACROS.with(|macros| {
        let mut macros = macros.borrow_mut();
        macros.clear();
        macros.extend(macros::builtin().into_iter().map(Arc::new));
    });

    let tokens = lex(src).map_err(|err| format!("{:?}", err))?.1;

    match program(&tokens) {
        Ok(([], defs)) => Ok(defs),
        Ok((i, _)) => Err(print_error(src, &tokens, i.first().unwrap())),
        Err(err) => Err(match err {
            nom::Err::Incomplete(_) => format!("unexpected EOS"),
            nom::Err::Error(err) | nom::Err::Failure(err) => {
                if let Some(token) = err.input.first() {
                    print_error(src, &tokens, token)
                } else {
                    format!("unexpected EOS")
                }
            }
        }),
    }
}

fn print_error(source: &str, tokens: &[Token<&str>], token: &Token<&str>) -> String {
    let mut ti = tokens.iter().position(|t| t == token).unwrap();
    let mut pos = 0;
    while ti > 0 {
        match tokens[ti] {
            Token::Keyword(s) | Token::Identifier(s) | Token::Operator(s) => {
                pos = s.as_ptr() as usize - source.as_ptr() as usize;
                break;
            }
            Token::Paren(_) | Token::String(_) | Token::Int(_) | Token::Float(_) => {}
        };
        ti -= 1;
    }
    format!(
        "unexpected: {:?}",
        &source[pos..source.len().min(pos + 100)]
    )
}

fn program<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Vec<Definition>> {
    let (i, defs) = definitions(i)?;
    Ok((i, defs))
}

fn definitions<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Vec<Definition>> {
    map(
        many0(alt((
            map(
                alt((variable_def, function_def, struct_def, module_def)),
                |def| vec![def],
            ),
            |i| {
                let (i, d) = macro_def(i)?;
                MACROS.with(|macros| {
                    let mut macros = macros.borrow_mut();
                    macros.push(Arc::new(d));
                });
                Ok((i, vec![]))
            },
            |i| {
                let (i, macro_call) = macro_call(i)?;
                let src = MACROS.with(|macros| {
                    let macros = macros.borrow();
                    let macro_ = macros
                        .iter()
                        .find(|m| m.name() == macro_call.name)
                        .expect("macro not found"); // TODO error mes
                    macro_.expand(&macro_call.arg).unwrap()
                });
                let (_, defs) = definitions(&src).unwrap(); // TODO
                Ok((i, defs))
            },
        ))),
        |defs| defs.into_iter().flatten().collect(),
    )(i)
}

fn variable_def<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Definition> {
    let (i, mutable) = alt((value(false, keyword("let")), value(true, keyword("var"))))(i)?;

    cut(move |i| {
        let (i, name) = identifier(i)?;
        let (i, _) = op("=")(i)?;
        let (i, expr) = control(i)?;
        let (i, _) = op(";")(i)?;
        Ok((
            i,
            Definition::Variable {
                name,
                mutable,
                expr,
            },
        ))
    })(i)
}

fn function_def<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Definition> {
    let (i, _) = keyword("fn")(i)?;
    cut(alt((
        |i| {
            let (i, receiver) = indexing(i)?;
            let (i, name) = identifier(i)?;
            let (i, _) = paren('(')(i)?;
            let (i, args) = separated_list0(op(","), identifier)(i)?;
            let (i, _) = paren(')')(i)?;
            let (i, body) = body(i)?;
            Ok((
                i,
                Definition::Method {
                    receiver,
                    function: Function { name, args, body },
                },
            ))
        },
        |i| {
            let (i, name) = identifier(i)?;
            let (i, _) = paren('(')(i)?;
            let (i, args) = separated_list0(op(","), identifier)(i)?;
            let (i, _) = paren(')')(i)?;
            let (i, body) = body(i)?;
            Ok((i, Definition::Function(Function { name, args, body })))
        },
    )))(i)
}

fn struct_def<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Definition> {
    let (i, _) = keyword("struct")(i)?;
    cut(|i| {
        let (i, name) = identifier(i)?;
        let (i, _) = paren('{')(i)?;
        let (i, fields) = comma_separated_list0(identifier)(i)?;
        let (i, _) = paren('}')(i)?;
        Ok((
            i,
            Definition::Struct {
                name,
                fields: fields.into_iter().map(|f| (f, true)).collect(),
            },
        ))
    })(i)
}

fn module_def<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Definition> {
    let (i, _) = keyword("mod")(i)?;
    cut(|i| {
        let (i, name) = identifier(i)?;
        let (i, _) = op(";")(i)?;
        Ok((i, Definition::Module(name)))
    })(i)
}

fn body<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = colon_if_not_keyword(i)?;
    control(i)
}

fn colon_if_not_keyword<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], ()> {
    alt((
        terminated(
            peek(alt((
                value((), any_keyword),
                value((), alt((paren('['), paren('{')))),
            ))),
            opt(op(":")),
        ),
        value((), op(":")),
    ))(i)
}

fn control<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    alt((
        // map(macro_call, Expression::MacroCall),
        |i| {
            let (i, macro_call) = macro_call(i)?;
            let src = MACROS.with(|macros| {
                let macros = macros.borrow();
                let macro_ = macros
                    .iter()
                    .find(|m| m.name() == macro_call.name)
                    .expect("macro not found");
                macro_.expand(&macro_call.arg).unwrap()
            });
            let (_, expr) = control(&src).unwrap(); // TODO
            Ok((i, expr))
        },
        return_expr,
        |i| {
            let (i, _) = keyword("break")(i)?;
            let (i, label) = opt(label)(i)?;
            let (i, expr) = cut(opt(or_cond))(i)?;
            Ok((
                i,
                Expression::Break {
                    label: label.unwrap_or_default(),
                    expr: expr.map(Box::new),
                },
            ))
        },
        |i| {
            let (i, _) = keyword("continue")(i)?;
            let (i, label) = opt(label)(i)?;
            Ok((
                i,
                Expression::Continue {
                    label: label.unwrap_or_default(),
                },
            ))
        },
        or_cond,
    ))(i)
}

fn return_expr<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = keyword("return")(i)?;
    let (i, value) = cut(opt(control))(i)?;
    Ok((
        i,
        Expression::Return {
            expr: value.map(Box::new),
        },
    ))
}

fn or_cond<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    left_assoc(
        and_cond,
        op("||"),
        |op, left, right| {
            op_call(
                match op.as_str() {
                    "||" => "__or",
                    _ => unreachable!(),
                },
                vec![left, right],
            )
        },
        i,
    )
}

fn and_cond<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    left_assoc(
        equality,
        op("&&"),
        |op, left, right| {
            op_call(
                match op.as_str() {
                    "&&" => "__and",
                    _ => unreachable!(),
                },
                vec![left, right],
            )
        },
        i,
    )
}

fn equality<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (mut i, mut expr) = additive(i)?;

    loop {
        let (i_, right) = opt(|i| {
            let (i, op) = alt((op("=="), op("!="), op(">="), op(">"), op("<="), op("<")))(i)?;
            let (i, right) = additive(i)?;
            Ok((i, (op, right)))
        })(i)?;
        i = i_;

        if let Some((op, right)) = right {
            expr = op_call(
                match op.as_str() {
                    "==" => "__eq",
                    "!=" => "__ne",
                    ">" => "__gt",
                    ">=" => "__ge",
                    "<" => "__lt",
                    "<=" => "__le",
                    _ => unreachable!(),
                },
                vec![expr, right],
            );
        } else {
            break;
        }
    }

    Ok((i, expr))
}

fn additive<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    left_assoc(
        multicative,
        alt((op("+"), op("-"))),
        |op, left, right| match op.as_str() {
            "+" => op_call("__add", vec![left, right]),
            "-" => op_call("__sub", vec![left, right]),
            _ => unreachable!(),
        },
        i,
    )
}

fn multicative<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    left_assoc(
        unary,
        alt((op("*"), op("/"), op("%"))),
        |op, left, right| match op.as_str() {
            "*" => op_call("__mul", vec![left, right]),
            "/" => op_call("__div", vec![left, right]),
            "%" => op_call("__rem", vec![left, right]),
            _ => unreachable!(),
        },
        i,
    )
}

fn unary<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], Expression> {
    alt((
        |i: &'a [Token<S>]| {
            let (i, op) = alt((op("-"), op("!")))(i)?;
            let (i, expr) = unary(i)?;
            Ok((
                i,
                op_call(
                    match op.as_str() {
                        "-" => "__neg",
                        "!" => "__not",
                        _ => unreachable!(),
                    },
                    vec![expr],
                ),
            ))
        },
        indexing,
    ))(i)
}

fn indexing<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (mut i, mut expr) = factor(i)?;
    loop {
        if let (i_, Some(index)) = opt(|i| {
            let (i, _) = paren('[')(i)?;
            let (i, index) = control(i)?;
            let (i, _) = paren(']')(i)?;
            Ok((i, index))
        })(i)?
        {
            i = i_;
            expr = op_call("__index", vec![expr, index]);
        } else if let (i_, Some(args)) = opt(|i| {
            let (i, _) = paren('(')(i)?;
            let (i, args) = comma_separated_list0(vec_append)(i)?;
            let (i, _) = paren(')')(i)?;
            Ok((i, args))
        })(i)?
        {
            i = i_;
            expr = Expression::Call {
                callee: Box::new(expr),
                args,
            };
        } else if let (i_, Some(appends)) = opt(|i| {
            let (i, _) = not(block)(i)?;
            let (i, _) = paren('{')(i)?;
            let (i, appends) = comma_separated_list0(dict_append)(i)?;
            let (i, _) = paren('}')(i)?;
            Ok((i, appends))
        })(i)?
        {
            i = i_;
            expr = Expression::Struct {
                constructor: Box::new(expr),
                appends,
            };
        } else if let (i_, Some(ident)) = opt(|i| {
            let (i, _) = op(".")(i)?;
            let (i, ident) = identifier(i)?;
            Ok((i, ident))
        })(i)?
        {
            i = i_;
            expr = op_call(
                "__index",
                vec![
                    expr,
                    Expression::Literal {
                        value: Literal::String(ident),
                    },
                ],
            );
        } else {
            break;
        }
    }
    Ok((i, expr))
}

fn factor<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    alt((
        |i| {
            let (i, _) = paren('(')(i)?;
            let (i, expr) = control(i)?;
            let (i, _) = paren(')')(i)?;
            Ok((i, expr))
        },
        block,
        closure,
        vec,
        dict,
        tuple,
        map(literal, |value| Expression::Literal { value }),
        map(identifier, |name| Expression::Variable { name }),
        cond_if,
        loop_,
        while_,
        match_expr,
    ))(i)
}

fn closure<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    alt((closure_fn, closure_pipe))(i)
}

fn closure_fn<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = keyword("fn")(i)?;
    let (i, _) = paren('(')(i)?;
    let (i, args) = separated_list0(op(","), identifier)(i)?;
    let (i, _) = paren(')')(i)?;
    let (i, body) = body(i)?;
    Ok((
        i,
        Expression::Closure(Box::new(Function {
            name: intern(""),
            args,
            body,
        })),
    ))
}

fn closure_pipe<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, args) = alt((value(vec![], op("||")), |i| {
        let (i, _) = op("|")(i)?;
        let (i, args) = separated_list0(op(","), identifier)(i)?;
        let (i, _) = op("|")(i)?;
        Ok((i, args))
    }))(i)?;
    let (i, body) = cut(control)(i)?;
    Ok((
        i,
        Expression::Closure(Box::new(Function {
            name: intern(""),
            args,
            body,
        })),
    ))
}

fn cond_if<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = keyword("if")(i)?;
    cut(|i| {
        let (i, condition) = control(i)?;
        let (i, then) = body(i)?;
        let (i, else_) = opt(|i| {
            let (i, _) = keyword("else")(i)?;
            let (i, else_) = body(i)?;
            Ok((i, else_))
        })(i)?;
        Ok((
            i,
            Expression::If {
                condition: Box::new(condition),
                then: Box::new(then),
                else_: else_.map(Box::new),
            },
        ))
    })(i)
}

fn loop_<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, label) = opt(labeled)(i)?;
    let (i, _) = keyword("loop")(i)?;

    let label = label.unwrap_or_default();
    cut(move |i| {
        let (i, body) = body(i)?;
        Ok((
            i,
            Expression::Labeled {
                label: label.clone(),
                body: Box::new(Expression::Loop {
                    body: Box::new(body),
                }),
            },
        ))
    })(i)
}

fn while_<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, label) = opt(labeled)(i)?;
    let (i, _) = keyword("while")(i)?;

    let label = label.unwrap_or_default();
    cut(move |i| {
        let (i, condition) = control(i)?;

        let (i, body) = body(i)?;
        Ok((
            i,
            Expression::Labeled {
                label: label.clone(),
                body: Box::new(Expression::Loop {
                    body: Box::new(Expression::If {
                        condition: Box::new(condition),
                        then: Box::new(body),
                        else_: Some(Box::new(Expression::Break {
                            label: label.clone(),
                            expr: None,
                        })),
                    }),
                }),
            },
        ))
    })(i)
}

fn labeled<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Str> {
    let (i, label) = label(i)?;
    let (i, _) = op(":")(i)?;
    Ok((i, label))
}

fn block<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = paren('{')(i)?;
    let (i, statements) = many0(statement_semicolon)(i)?;
    let (i, expr) = opt(control)(i)?;
    let (i, _) = paren('}')(i)?;

    if statements.is_empty() && expr.is_some() {
        return Ok((i, expr.unwrap()));
    }

    Ok((
        i,
        Expression::Block(Block {
            statements,
            expr: expr.map(Box::new),
        }),
    ))
}

fn match_expr<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = keyword("match")(i)?;
    cut(|i| {
        let (i, expr) = control(i)?;
        let (i, _) = paren('{')(i)?;
        let (i, arms) = comma_separated_list0(match_arm)(i)?;
        let (i, _) = paren('}')(i)?;
        Ok((
            i,
            Expression::Match {
                expr: Box::new(expr),
                arms,
            },
        ))
    })(i)
}

fn match_arm<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], (Pattern, Expression)> {
    let (i, pattern) = pattern(i)?;
    let (i, _) = op("=>")(i)?;
    let (i, body) = control(i)?;
    Ok((i, (pattern, body)))
}

fn pattern<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Pattern> {
    alt((
        map(wildcard, |_| Pattern::Wildcard),
        |i| {
            let (i, _) = paren('[')(i)?;
            let (i, exprs) = comma_separated_list0(pattern)(i)?;
            let (i, allow_tail) = opt(op(".."))(i)?;
            let (i, _) = paren(']')(i)?;
            Ok((
                i,
                Pattern::Vec {
                    values: exprs,
                    allow_tail: allow_tail.is_some(),
                },
            ))
        },
        |i| {
            let (i, constructor) = opt(identifier)(i)?;
            let (i, _) = paren('{')(i)?;
            let (i, fields) = comma_separated_list0(|i| {
                let (i, name) = identifier(i)?;
                let (i, _) = op(":")(i)?;
                let (i, pattern) = pattern(i)?;
                Ok((i, (name, pattern)))
            })(i)?;
            let (i, _) = paren('}')(i)?;
            let fields = fields.into_iter().map(|(name, pat)| (name, pat)).collect();
            Ok((
                i,
                if let Some(constructor) = constructor {
                    Pattern::Struct {
                        constructor,
                        fields,
                    }
                } else {
                    Pattern::Dict(fields)
                },
            ))
        },
        |i| {
            let (i, name) = identifier(i)?;
            let (i, type_) = opt(preceded(op(":"), pattern))(i)?;
            Ok((
                i,
                Pattern::Variable {
                    name,
                    type_: type_.map(Box::new),
                },
            ))
        },
        map(literal, |value| Pattern::Literal(value)),
    ))(i)
}

fn statement<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Statement> {
    alt((let_statement, assign_statement))(i)
}

fn let_statement<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Statement> {
    let (i, mutable) = alt((value(false, keyword("let")), value(true, keyword("var"))))(i)?;

    cut(move |i| {
        let (i, name) = identifier(i)?;
        let (i, _) = op("=")(i)?;
        let (i, value) = control(i)?;
        Ok((
            i,
            Statement::Let {
                name,
                mutable,
                expr: value,
            },
        ))
    })(i)
}

fn assign_statement<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Statement> {
    let (i, expr) = indexing(i)?;
    match expr {
        Expression::Op { name, mut args } if name.as_str() == "__index" => {
            let field = args.pop().unwrap();
            let dict = args.pop().unwrap();

            let (i, o) = opt(assignable_op)(i)?;
            let (i, _) = op("=")(i)?;
            let (i, expr) = control(i)?;
            Ok((
                i,
                Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op: o.map(op_to_str),
                },
            ))
        }
        Expression::Variable { name } => {
            let (i, o) = opt(assignable_op)(i)?;
            let (i, _) = op("=")(i)?;
            let (i, expr) = control(i)?;
            Ok((
                i,
                Statement::Assign {
                    name,
                    expr,
                    op: o.map(op_to_str),
                },
            ))
        }
        _ => return fail(i),
    }
}

fn assignable_op<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], &S> {
    alt((op("+"), op("-"), op("*"), op("/"), op("%")))(i)
}

fn op_to_str<S: AsStr>(token: &S) -> Str {
    intern(match token.as_str() {
        "+" => "__add",
        "-" => "__sub",
        "*" => "__mul",
        "/" => "__div",
        "%" => "__rem",
        _ => unreachable!(),
    })
}

fn statement_semicolon<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Statement> {
    let (i, stmt) = alt((
        statement,
        map(control, |expr| Statement::Expression { expr }),
    ))(i)?;
    let (i, _) = op(";")(i)?;
    Ok((i, stmt))
}

fn vec<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = paren('[')(i)?;
    let (i, appends) = comma_separated_list0(vec_append)(i)?;
    let (i, _) = paren(']')(i)?;
    Ok((i, Expression::Vec { appends }))
}

fn dict<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = paren('{')(i)?;
    let (i, appends) = comma_separated_list0(dict_append)(i)?;
    let (i, _) = paren('}')(i)?;
    Ok((i, Expression::Dict { appends }))
}

fn vec_append<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], VecAppend> {
    alt((
        |i| {
            let (i, expr) = control(i)?;
            Ok((i, VecAppend::Element(expr)))
        },
        |i| {
            let (i, _) = op("..")(i)?;
            let (i, expr) = control(i)?;
            Ok((i, VecAppend::Spread(expr)))
        },
    ))(i)
}

fn dict_append<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], DictAppend> {
    alt((
        |i| {
            let (i, name) = identifier(i)?;
            let (i, _) = op(":")(i)?;
            let (i, expr) = control(i)?;
            Ok((i, DictAppend::Field(name, expr)))
        },
        |i| {
            let (i, name) = identifier(i)?;
            Ok((
                i,
                DictAppend::Field(name.clone(), Expression::Variable { name }),
            ))
        },
        |i| {
            let (i, _) = op("..")(i)?;
            let (i, expr) = control(i)?;
            Ok((i, DictAppend::Spread(expr)))
        },
    ))(i)
}

fn tuple<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Expression> {
    let (i, _) = paren('(')(i)?;
    let (i, exprs) = separated_list0(op(","), control)(i)?;

    if exprs.is_empty() {
        return fail(i);
    }

    let (i, _) = if exprs.len() == 1 {
        let (i, _) = op(",")(i)?;
        (i, ())
    } else {
        (i, ())
    };
    let (i, _) = paren(')')(i)?;
    Ok((i, Expression::Tuple { exprs }))
}

fn literal<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Literal> {
    match i.first() {
        Some(Token::String(s)) => Ok((&i[1..], Literal::String(s.clone()))),
        Some(Token::Int(v)) => Ok((&i[1..], Literal::Int(*v))),
        Some(Token::Float(v)) => Ok((&i[1..], Literal::Float(*v))),
        Some(Token::Keyword(k)) if k.as_str() == "true" => Ok((&i[1..], Literal::Bool(true))),
        Some(Token::Keyword(k)) if k.as_str() == "false" => Ok((&i[1..], Literal::Bool(false))),
        Some(Token::Keyword(k)) if k.as_str() == "null" => Ok((&i[1..], Literal::Null)),
        Some(Token::Paren('(')) => {
            let (i, _) = paren('(')(i)?;
            let (i, _) = paren(')')(i)?;
            Ok((i, Literal::Unit))
        }
        _ => fail(i),
    }
}

fn label<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Str> {
    let (i, _) = op("'")(i)?;
    let (i, s) = identifier(i)?;
    Ok((i, s))
}

fn identifier<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], Str> {
    if let Some(Token::Identifier(ident)) = i.first() {
        return Ok((&i[1..], ident.interned()));
    }
    fail(i)
}

fn paren<S: AsStr>(name: char) -> impl FnMut(&[Token<S>]) -> IResult<&[Token<S>], Token<S>> {
    move |i| {
        if let Some(Token::Paren(c)) = i.first() {
            if *c == name {
                return Ok((&i[1..], Token::Paren(name)));
            }
        }
        fail(i)
    }
}

fn wildcard<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], ()> {
    if let Some(Token::Identifier(s)) = i.first() {
        if s.as_str() == "_" {
            return Ok((&i[1..], ()));
        }
    }
    fail(i)
}

fn keyword<S: AsStr>(name: &'static str) -> impl FnMut(&[Token<S>]) -> IResult<&[Token<S>], ()> {
    move |i| {
        if let Some(Token::Keyword(k)) = i.first() {
            if k.as_str() == name {
                return Ok((&i[1..], ()));
            }
        }
        fail(i)
    }
}

fn any_keyword<S: AsStr>(i: &[Token<S>]) -> IResult<&[Token<S>], ()> {
    alt((
        keyword("fn"),
        keyword("struct"),
        keyword("let"),
        keyword("var"),
        keyword("return"),
        keyword("if"),
        keyword("else"),
        keyword("loop"),
        keyword("while"),
        keyword("for"),
        keyword("match"),
        keyword("break"),
        keyword("continue"),
    ))(i)
}

fn op<S: AsStr>(name: &'static str) -> impl FnMut(&[Token<S>]) -> IResult<&[Token<S>], &S> {
    move |i| {
        if let Some(Token::Operator(k)) = i.first() {
            if k.as_str() == name {
                return Ok((&i[1..], k));
            }
        }
        fail(i)
    }
}

fn left_assoc<'a, C, OP, S: AsStr>(
    mut child: impl FnMut(&'a [Token<S>]) -> IResult<&'a [Token<S>], C>,
    mut op: impl FnMut(&'a [Token<S>]) -> IResult<&'a [Token<S>], OP>,
    merge: impl Fn(OP, C, C) -> C,
    i: &'a [Token<S>],
) -> IResult<&'a [Token<S>], C> {
    let (i, mut expr) = child(i)?;
    let (i, exprs) = many0(move |i| {
        let (i, op) = op(i)?;
        let (i, right) = child(i)?;
        Ok((i, (op, right)))
    })(i)?;
    for (op, right) in exprs {
        expr = merge(op, expr, right);
    }
    Ok((i, expr))
}

fn comma_separated_list0<'a, F, O, S: AsStr + 'a>(
    f: F,
) -> impl FnMut(&'a [Token<S>]) -> IResult<&'a [Token<S>], Vec<O>>
where
    F: Fn(&'a [Token<S>]) -> IResult<&'a [Token<S>], O>,
{
    terminated(separated_list0(op(","), f), opt(op(",")))
}

fn op_call(name: &str, args: Vec<Expression>) -> Expression {
    Expression::Op {
        name: intern(name),
        args,
    }
}

#[test]
fn test() {
    let inputs = [
        r#"
fn add(a, b) {
    add(a, b, 123)
}
"#,
        r#"
fn f(a, b) {
    let c = add(a, b);
    return if true {123};
}
"#,
        r#"
fn f() {
    return 1 + 2 * 3 - 4 / 5;
}
"#,
        r#"
let f = fn(a) {return 123;};
"#,
        r#"
let f = |a| {
    return 123;
};
let f = || 123;
"#,
        r#"
/* comment */
fn main() {
    1 // comment!!!
}"#,
        r#"
fn fib(n)
    if 2 > n:
        1
    else:
        fib(n - 1) + fib(n - 2)

fn main():
    [fib(1), fib(2)]
"#,
        r#"
fn f()
    loop
        if true
            break
        else
            continue"#,
        r#"
let a = if a if b:1 else:2 else:3;"#,
        r#"
fn main() {
    1
    ()
}
"#,
        r#"
fn main(): breaking
"#,
    ];
    for input in &inputs {
        let result = parse(input);
        gilder::assert_golden!(format!("{:?}", result));
    }
}

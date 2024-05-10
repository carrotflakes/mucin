pub mod macros;
mod string;

use std::sync::Arc;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, digit1, one_of},
    combinator::{cut, eof, fail, map, not, opt, peek, recognize, value},
    multi::{many0, many0_count, many1, separated_list0},
    number::complete::double,
    sequence::{pair, preceded, terminated},
    IResult,
};

use crate::{
    model::{
        Block, Definition, DictAppend, Expression, Function, Literal, Pattern, Statement, VecAppend,
    },
    string::{intern, Str},
};
use string::parse_string as string;

use self::macros::{
    parser::{macro_call, macro_def},
    Macro,
};

thread_local! {
    static MACROS: std::cell::RefCell<Vec<Arc<Macro>>> = std::cell::RefCell::new(Vec::new());
}

pub fn parse(i: &str) -> Result<Vec<Definition>, String> {
    MACROS.with(|macros| {
        let mut macros = macros.borrow_mut();
        macros.clear();
        macros.extend(macros::builtin().into_iter().map(Arc::new));
    });

    match program(i) {
        Ok(("", defs)) => Ok(defs),
        Ok((i, _)) => Err(format!("parse failed at: {:?}", i)),
        Err(err) => Err(format!("{:?}", err)),
    }
}

fn program(i: &str) -> IResult<&str, Vec<Definition>> {
    let (i, _) = sp(i)?;
    let (i, defs) = definitions(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = eof(i)?;
    Ok((i, defs))
}

fn definitions(i: &str) -> IResult<&str, Vec<Definition>> {
    map(
        many0(preceded(
            sp,
            alt((
                map(
                    alt((variable_def, function_def, struct_def, module_def)),
                    |def| vec![def],
                ),
                |i| {
                    let (i, d) = macro_def(i)?;
                    MACROS.with(|macros| {
                        let mut macros = macros.borrow_mut();
                        macros.push(Arc::new(Macro::Rules(d)));
                    });
                    Ok((i, vec![]))
                },
                |i| {
                    let (i, (name, token)) = macro_call(i)?;
                    let src = MACROS.with(|macros| {
                        let macros = macros.borrow();
                        let macro_ = macros
                            .iter()
                            .find(|m| m.name() == name)
                            .expect("macro not found"); // TODO error mes
                        macro_.expand(&token).unwrap()
                    });
                    let (_, defs) = definitions(&src).unwrap(); // TODO
                    Ok((i, defs))
                },
            )),
        )),
        |defs| defs.into_iter().flatten().collect(),
    )(i)
}

fn variable_def(i: &str) -> IResult<&str, Definition> {
    let (i, keyword) = alt((keyword("let"), keyword("var")))(i)?;
    let (i, _) = sp(i)?;

    let mutable = keyword == "var";
    cut(move |i| {
        let (i, name) = identifier(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("=")(i)?;
        let (i, _) = sp(i)?;
        let (i, expr) = control(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag(";")(i)?;
        Ok((
            i,
            Definition::Variable {
                name: intern(name),
                mutable,
                expr,
            },
        ))
    })(i)
}

fn function_def(i: &str) -> IResult<&str, Definition> {
    let (i, _) = keyword("fn")(i)?;
    let (i, _) = sp(i)?;
    cut(alt((
        |i| {
            let (i, receiver) = indexing(i)?;
            let (i, _) = sp(i)?;
            let (i, name) = identifier(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, _) = sp(i)?;
            let (i, args) = separated_list0(preceded(sp, char(',')), preceded(sp, identifier))(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = sp(i)?;
            let (i, body) = body(i)?;
            Ok((
                i,
                Definition::Method {
                    receiver,
                    function: Function {
                        name: intern(name),
                        args: args.into_iter().map(intern).collect(),
                        body,
                    },
                },
            ))
        },
        |i| {
            let (i, name) = identifier(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, _) = sp(i)?;
            let (i, args) = separated_list0(preceded(sp, char(',')), preceded(sp, identifier))(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = sp(i)?;
            let (i, body) = body(i)?;
            Ok((
                i,
                Definition::Function(Function {
                    name: intern(name),
                    args: args.into_iter().map(intern).collect(),
                    body,
                }),
            ))
        },
    )))(i)
}

fn struct_def(i: &str) -> IResult<&str, Definition> {
    let (i, _) = keyword("struct")(i)?;
    let (i, _) = sp(i)?;
    cut(|i| {
        let (i, name) = identifier(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("{")(i)?;
        let (i, _) = sp(i)?;
        let (i, fields) = comma_separated_list0(identifier)(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("}")(i)?;
        Ok((
            i,
            Definition::Struct {
                name: intern(name),
                fields: fields.into_iter().map(|f| (intern(f), true)).collect(),
            },
        ))
    })(i)
}

fn module_def(i: &str) -> IResult<&str, Definition> {
    let (i, _) = keyword("mod")(i)?;
    let (i, _) = sp(i)?;
    cut(|i| {
        let (i, name) = identifier(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag(";")(i)?;
        Ok((i, Definition::Module(intern(name))))
    })(i)
}

fn body(i: &str) -> IResult<&str, Expression> {
    let (i, _) = colon_if_not_keyword(i)?;
    let (i, _) = sp(i)?;
    control(i)
}

fn colon_if_not_keyword(i: &str) -> IResult<&str, ()> {
    alt((
        terminated(
            peek(alt((value((), any_keyword), value((), one_of("[{"))))),
            opt(tag(":")),
        ),
        value((), tag(":")),
    ))(i)
}

fn control(i: &str) -> IResult<&str, Expression> {
    alt((
        |i| {
            let (i, (name, token)) = macro_call(i)?;
            let src = MACROS.with(|macros| {
                let macros = macros.borrow();
                let macro_ = macros
                    .iter()
                    .find(|m| m.name() == name)
                    .expect("macro not found");
                macro_.expand(&token).unwrap()
            });
            let (_, expr) = control(&src).unwrap(); // TODO
            Ok((i, expr))
        },
        return_expr,
        |i| {
            let (i, _) = keyword("break")(i)?;
            let (i, _) = sp(i)?;
            let (i, label) = opt(terminated(label, sp))(i)?;
            let (i, expr) = cut(opt(or_cond))(i)?;
            Ok((
                i,
                Expression::Break {
                    label: label.map(intern).unwrap_or_default(),
                    expr: expr.map(Box::new),
                },
            ))
        },
        |i| {
            let (i, _) = keyword("continue")(i)?;
            let (i, _) = sp(i)?;
            let (i, label) = opt(terminated(label, sp))(i)?;
            Ok((
                i,
                Expression::Continue {
                    label: label.map(intern).unwrap_or_default(),
                },
            ))
        },
        or_cond,
    ))(i)
}

fn return_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = keyword("return")(i)?;
    let (i, _) = sp(i)?;
    let (i, value) = cut(opt(control))(i)?;
    Ok((
        i,
        Expression::Return {
            expr: value.map(Box::new),
        },
    ))
}

fn or_cond(i: &str) -> IResult<&str, Expression> {
    left_assoc(
        and_cond,
        tag("||"),
        |op, left, right| {
            op_call(
                match op {
                    "||" => "__or",
                    _ => unreachable!(),
                },
                vec![left, right],
            )
        },
        i,
    )
}

fn and_cond(i: &str) -> IResult<&str, Expression> {
    left_assoc(
        equality,
        tag("&&"),
        |op, left, right| {
            op_call(
                match op {
                    "&&" => "__and",
                    _ => unreachable!(),
                },
                vec![left, right],
            )
        },
        i,
    )
}

fn equality(i: &str) -> IResult<&str, Expression> {
    let (mut i, mut expr) = additive(i)?;

    loop {
        let (i_, right) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, op) = alt((
                tag("=="),
                tag("!="),
                tag(">="),
                tag(">"),
                tag("<="),
                tag("<"),
            ))(i)?;
            let (i, _) = sp(i)?;
            let (i, right) = additive(i)?;
            Ok((i, (op, right)))
        })(i)?;
        i = i_;

        if let Some((op, right)) = right {
            expr = op_call(
                match op {
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

fn additive(i: &str) -> IResult<&str, Expression> {
    left_assoc(
        multicative,
        alt((tag("+"), tag("-"))),
        |op, left, right| match op {
            "+" => op_call("__add", vec![left, right]),
            "-" => op_call("__sub", vec![left, right]),
            _ => unreachable!(),
        },
        i,
    )
}

fn multicative(i: &str) -> IResult<&str, Expression> {
    left_assoc(
        unary,
        alt((tag("*"), tag("/"), tag("%"))),
        |op, left, right| match op {
            "*" => op_call("__mul", vec![left, right]),
            "/" => op_call("__div", vec![left, right]),
            "%" => op_call("__rem", vec![left, right]),
            _ => unreachable!(),
        },
        i,
    )
}

fn unary(i: &str) -> IResult<&str, Expression> {
    alt((
        |i| {
            let (i, op) = alt((tag("-"), tag("!")))(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = unary(i)?;
            Ok((
                i,
                op_call(
                    match op {
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

fn indexing(i: &str) -> IResult<&str, Expression> {
    let (mut i, mut expr) = factor(i)?;
    loop {
        if let (i_, Some(index)) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, _) = tag("[")(i)?;
            let (i, _) = sp(i)?;
            let (i, index) = control(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("]")(i)?;
            Ok((i, index))
        })(i)?
        {
            i = i_;
            expr = op_call("__index", vec![expr, index]);
        } else if let (i_, Some(args)) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, _) = sp(i)?;
            let (i, args) = comma_separated_list0(vec_append)(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, args))
        })(i)?
        {
            i = i_;
            expr = Expression::Call {
                callee: Box::new(expr),
                args,
            };
        } else if let (i_, Some(appends)) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, _) = not(block)(i)?;
            let (i, _) = tag("{")(i)?;
            let (i, _) = sp(i)?;
            let (i, appends) = comma_separated_list0(dict_append)(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("}")(i)?;
            Ok((i, appends))
        })(i)?
        {
            i = i_;
            expr = Expression::Struct {
                constructor: Box::new(expr),
                appends,
            };
        } else if let (i_, Some(ident)) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, _) = tag(".")(i)?;
            let (i, _) = sp(i)?;
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
                        value: Literal::String(intern(ident)),
                    },
                ],
            );
        } else {
            break;
        }
    }
    Ok((i, expr))
}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((
        |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, expr))
        },
        block,
        closure,
        vec,
        dict,
        tuple,
        map(literal, |value| Expression::Literal { value }),
        map(identifier, |name| Expression::Variable {
            name: intern(name),
        }),
        cond_if,
        loop_,
        while_,
        match_expr,
    ))(i)
}

fn closure(i: &str) -> IResult<&str, Expression> {
    alt((closure_fn, closure_pipe))(i)
}

fn closure_fn(i: &str) -> IResult<&str, Expression> {
    let (i, _) = keyword("fn")(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("(")(i)?;
    let (i, _) = sp(i)?;
    let (i, args) = separated_list0(preceded(sp, char(',')), preceded(sp, identifier))(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(")")(i)?;
    let (i, _) = sp(i)?;
    let (i, body) = body(i)?;
    Ok((
        i,
        Expression::Closure(Box::new(Function {
            name: intern(""),
            args: args.into_iter().map(intern).collect(),
            body,
        })),
    ))
}

fn closure_pipe(i: &str) -> IResult<&str, Expression> {
    let (i, _) = tag("|")(i)?;
    let (i, _) = sp(i)?;
    let (i, args) = separated_list0(preceded(sp, char(',')), preceded(sp, identifier))(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("|")(i)?;
    let (i, _) = sp(i)?;
    let (i, body) = cut(control)(i)?;
    Ok((
        i,
        Expression::Closure(Box::new(Function {
            name: intern(""),
            args: args.into_iter().map(intern).collect(),
            body,
        })),
    ))
}

fn cond_if(i: &str) -> IResult<&str, Expression> {
    let (i, _) = keyword("if")(i)?;
    let (i, _) = sp(i)?;
    cut(|i| {
        let (i, condition) = control(i)?;
        let (i, _) = sp(i)?;
        let (i, then) = body(i)?;
        let (i, else_) = opt(|i| {
            let (i, _) = sp(i)?;
            let (i, _) = keyword("else")(i)?;
            let (i, _) = sp(i)?;
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

fn loop_(i: &str) -> IResult<&str, Expression> {
    let (i, label) = opt(terminated(labeled, sp))(i)?;
    let (i, _) = keyword("loop")(i)?;
    let (i, _) = sp(i)?;

    let label = label.unwrap_or_default();
    cut(move |i| {
        let (i, body) = body(i)?;
        Ok((
            i,
            Expression::Labeled {
                label: intern(label),
                body: Box::new(Expression::Loop {
                    body: Box::new(body),
                }),
            },
        ))
    })(i)
}

fn while_(i: &str) -> IResult<&str, Expression> {
    let (i, label) = opt(terminated(labeled, sp))(i)?;
    let (i, _) = keyword("while")(i)?;
    let (i, _) = sp(i)?;

    let label = label.unwrap_or_default();
    cut(move |i| {
        let (i, condition) = control(i)?;
        let (i, _) = sp(i)?;
        let (i, body) = body(i)?;
        Ok((
            i,
            Expression::Labeled {
                label: intern(label),
                body: Box::new(Expression::Loop {
                    body: Box::new(Expression::If {
                        condition: Box::new(condition),
                        then: Box::new(body),
                        else_: Some(Box::new(Expression::Break {
                            label: intern(label),
                            expr: None,
                        })),
                    }),
                }),
            },
        ))
    })(i)
}

fn labeled(i: &str) -> IResult<&str, &str> {
    let (i, label) = label(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(":")(i)?;
    Ok((i, label))
}

fn block(i: &str) -> IResult<&str, Expression> {
    let (i, _) = tag("{")(i)?;
    let (i, statements) = many0(preceded(sp, statement_semicolon))(i)?;
    let (i, expr) = opt(preceded(sp, control))(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("}")(i)?;

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

fn match_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = keyword("match")(i)?;
    let (i, _) = sp(i)?;
    cut(|i| {
        let (i, expr) = control(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("{")(i)?;
        let (i, _) = sp(i)?;
        let (i, arms) = comma_separated_list0(match_arm)(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("}")(i)?;
        Ok((
            i,
            Expression::Match {
                expr: Box::new(expr),
                arms,
            },
        ))
    })(i)
}

fn match_arm(i: &str) -> IResult<&str, (Pattern, Expression)> {
    let (i, pattern) = pattern(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = sp(i)?;
    let (i, body) = control(i)?;
    Ok((i, (pattern, body)))
}

fn pattern(i: &str) -> IResult<&str, Pattern> {
    alt((
        map(tag("_"), |_| Pattern::Wildcard),
        |i| {
            let (i, _) = tag("[")(i)?;
            let (i, _) = sp(i)?;
            let (i, exprs) = comma_separated_list0(pattern)(i)?;
            let (i, _) = sp(i)?;
            let (i, allow_tail) = opt(tag(".."))(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("]")(i)?;
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
            let (i, _) = sp(i)?;
            let (i, _) = tag("{")(i)?;
            let (i, _) = sp(i)?;
            let (i, fields) = comma_separated_list0(|i| {
                let (i, name) = identifier(i)?;
                let (i, _) = sp(i)?;
                let (i, _) = tag(":")(i)?;
                let (i, _) = sp(i)?;
                let (i, pattern) = pattern(i)?;
                Ok((i, (name, pattern)))
            })(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("}")(i)?;
            let fields = fields
                .into_iter()
                .map(|(name, pat)| (intern(name), pat))
                .collect();
            Ok((
                i,
                if let Some(constructor) = constructor {
                    Pattern::Struct {
                        constructor: intern(constructor),
                        fields,
                    }
                } else {
                    Pattern::Dict(fields)
                },
            ))
        },
        |i| {
            let (i, name) = identifier(i)?;
            let (i, type_) = opt(preceded(sp, preceded(tag(":"), preceded(sp, pattern))))(i)?;
            Ok((
                i,
                Pattern::Variable {
                    name: intern(name),
                    type_: type_.map(Box::new),
                },
            ))
        },
        map(literal, |value| Pattern::Literal(value)),
    ))(i)
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((let_statement, assign_statement))(i)
}

fn let_statement(i: &str) -> IResult<&str, Statement> {
    let (i, keyword) = alt((keyword("let"), keyword("var")))(i)?;
    let (i, _) = sp(i)?;

    let mutable = keyword == "var";
    cut(move |i| {
        let (i, name) = identifier(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("=")(i)?;
        let (i, _) = sp(i)?;
        let (i, value) = control(i)?;
        Ok((
            i,
            Statement::Let {
                name: intern(name),
                mutable,
                expr: value,
            },
        ))
    })(i)
}

fn assign_statement(i: &str) -> IResult<&str, Statement> {
    let (i, expr) = indexing(i)?;
    match expr {
        Expression::Op { name, mut args } if name.as_str() == "__index" => {
            let field = args.pop().unwrap();
            let dict = args.pop().unwrap();

            let (i, _) = sp(i)?;
            let (i, op) = opt(assignable_op)(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            Ok((
                i,
                Statement::FieldAssign {
                    dict,
                    field,
                    expr,
                    op: op.map(op_to_str),
                },
            ))
        }
        Expression::Variable { name } => {
            let (i, _) = sp(i)?;
            let (i, op) = opt(assignable_op)(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            Ok((
                i,
                Statement::Assign {
                    name,
                    expr,
                    op: op.map(op_to_str),
                },
            ))
        }
        _ => return fail(i),
    }
}

fn assignable_op(i: &str) -> IResult<&str, &str> {
    alt((tag("+"), tag("-"), tag("*"), tag("/"), tag("%")))(i)
}

fn op_to_str(str: &str) -> Str {
    intern(match str {
        "+" => "__add",
        "-" => "__sub",
        "*" => "__mul",
        "/" => "__div",
        "%" => "__rem",
        _ => unreachable!(),
    })
}

fn statement_semicolon(i: &str) -> IResult<&str, Statement> {
    let (i, stmt) = alt((
        statement,
        map(control, |expr| Statement::Expression { expr }),
    ))(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag(";")(i)?;
    Ok((i, stmt))
}

fn vec(i: &str) -> IResult<&str, Expression> {
    let (i, _) = tag("[")(i)?;
    let (i, _) = sp(i)?;
    let (i, appends) = comma_separated_list0(vec_append)(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("]")(i)?;
    Ok((i, Expression::Vec { appends }))
}

fn dict(i: &str) -> IResult<&str, Expression> {
    let (i, _) = tag("{")(i)?;
    let (i, _) = sp(i)?;
    let (i, appends) = comma_separated_list0(dict_append)(i)?;
    let (i, _) = sp(i)?;
    let (i, _) = tag("}")(i)?;
    Ok((i, Expression::Dict { appends }))
}

fn vec_append(i: &str) -> IResult<&str, VecAppend> {
    alt((
        |i| {
            let (i, expr) = control(i)?;
            Ok((i, VecAppend::Element(expr)))
        },
        |i| {
            let (i, _) = tag("..")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            Ok((i, VecAppend::Spread(expr)))
        },
    ))(i)
}

fn dict_append(i: &str) -> IResult<&str, DictAppend> {
    alt((
        |i| {
            let (i, name) = identifier(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag(":")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            Ok((i, DictAppend::Field(intern(name), expr)))
        },
        |i| {
            let (i, name) = identifier(i)?;
            let name = intern(name);
            Ok((
                i,
                DictAppend::Field(name.clone(), Expression::Variable { name }),
            ))
        },
        |i| {
            let (i, _) = tag("..")(i)?;
            let (i, _) = sp(i)?;
            let (i, expr) = control(i)?;
            Ok((i, DictAppend::Spread(expr)))
        },
    ))(i)
}

fn tuple(i: &str) -> IResult<&str, Expression> {
    let (i, _) = tag("(")(i)?;
    let (i, _) = sp(i)?;
    let (i, exprs) = separated_list0(preceded(sp, char(',')), preceded(sp, control))(i)?;

    if exprs.is_empty() {
        return fail(i);
    }

    let (i, _) = if exprs.len() == 1 {
        let (i, _) = sp(i)?;
        let (i, _) = tag(",")(i)?;
        (i, ())
    } else {
        (i, ())
    };
    let (i, _) = sp(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((i, Expression::Tuple { exprs }))
}

pub fn literal(i: &str) -> IResult<&str, Literal> {
    alt((
        map(tag("()"), |_| Literal::Unit),
        map(string, |s| Literal::String(intern(&s))),
        map(terminated(digit1, not(char('.'))), |s: &str| {
            Literal::Int(s.parse().unwrap())
        }),
        map(double, |v| Literal::Float(v)),
        map(keyword("true"), |_| Literal::Bool(true)),
        map(keyword("false"), |_| Literal::Bool(false)),
        map(keyword("null"), |_| Literal::Null),
    ))(i)
}

fn label(i: &str) -> IResult<&str, &str> {
    let (i, _) = tag("'")(i)?;
    let (i, s) = identifier(i)?;
    Ok((i, s))
}

pub fn identifier(i: &str) -> IResult<&str, &str> {
    let (i, _) = not(any_keyword)(i)?;

    let (i, str) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(i)?;
    Ok((i, str))
}

fn keyword<'a>(name: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    terminated(tag(name), not(alt((alphanumeric1, tag("_")))))
}

fn any_keyword(i: &str) -> IResult<&str, &str> {
    terminated(
        alt((
            tag("fn"),
            tag("struct"),
            tag("let"),
            tag("var"),
            tag("return"),
            tag("if"),
            tag("else"),
            tag("loop"),
            tag("while"),
            tag("for"),
            tag("match"),
            tag("break"),
            tag("continue"),
        )),
        not(alt((alphanumeric1, tag("_")))),
    )(i)
}

pub fn sp(i: &str) -> IResult<&str, ()> {
    value((), opt(sp1))(i)
}

pub fn sp1(i: &str) -> IResult<&str, ()> {
    const SP_CHARS: &str = " \t\r\n";

    value((), many1(alt((value((), one_of(SP_CHARS)), comment))))(i)
}

pub fn comment(i: &str) -> IResult<&str, ()> {
    alt((
        value((), pair(tag("//"), is_not("\n\r"))),
        value((), pair(pair(tag("/*"), take_until("*/")), tag("*/"))),
    ))(i)
}

fn left_assoc<'a, C>(
    mut child: impl FnMut(&'a str) -> IResult<&'a str, C>,
    mut op: impl FnMut(&'a str) -> IResult<&'a str, &'a str>,
    merge: impl Fn(&'a str, C, C) -> C,
    i: &'a str,
) -> IResult<&'a str, C> {
    let (i, mut expr) = child(i)?;
    let (i, exprs) = many0(move |i| {
        let (i, _) = sp(i)?;
        let (i, op) = op(i)?;
        let (i, _) = sp(i)?;
        let (i, right) = child(i)?;
        Ok((i, (op, right)))
    })(i)?;
    for (op, right) in exprs {
        expr = merge(op, expr, right);
    }
    Ok((i, expr))
}

fn comma_separated_list0<'a, F, O>(f: F) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>>
where
    F: Fn(&'a str) -> IResult<&'a str, O>,
{
    terminated(
        separated_list0(preceded(sp, char(',')), preceded(sp, f)),
        opt(preceded(sp, char(','))),
    )
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
        let result = program(input);
        gilder::assert_golden!(format!("{:?}", result));
    }
}

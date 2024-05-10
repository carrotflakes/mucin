use crate::{
    model::*,
    native_fns::{NF_STRUCT_TYPE, NF_TYPEOF},
    string::{intern, unique_str},
};

pub fn convert_match(target: Expression, cases: Vec<(Pattern, Expression)>) -> Expression {
    let label_name = unique_str("#label");
    let target_var_name = unique_str("#var");
    let target_var = Expression::Variable {
        name: target_var_name.clone(),
    };

    Expression::Labeled {
        label: label_name.clone(),
        body: Box::new(Expression::Block(Block {
            statements: vec![Statement::Let {
                name: target_var_name,
                mutable: false,
                expr: target,
            }],
            expr: Some(Box::new(Expression::Block(Block {
                statements: cases
                    .into_iter()
                    .map(|(pattern, body)| Statement::Expression {
                        expr: pattern.to_expression(
                            target_var.clone(),
                            Expression::Break {
                                label: label_name.clone(),
                                expr: Some(Box::new(body)),
                            },
                        ),
                    })
                    .collect(),
                expr: None,
            }))),
        })),
    }
}

impl Pattern {
    fn to_expression(self, target_var: Expression, body: Expression) -> Expression {
        match self {
            Pattern::Wildcard => body,
            Pattern::Variable {
                name,
                type_: Some(type_),
            } => {
                let type_var_name = unique_str("#var");
                Expression::Block(Block {
                    statements: vec![
                        Statement::Let {
                            name: name.clone(),
                            mutable: false,
                            expr: target_var.clone(),
                        },
                        Statement::Let {
                            name: type_var_name.clone(),
                            mutable: false,
                            expr: Expression::Call {
                                callee: Box::new(Expression::StaticNativeFn {
                                    native_fn: &NF_TYPEOF,
                                }),
                                args: vec![VecAppend::Element(target_var.clone())],
                            },
                        },
                    ],
                    expr: Some(Box::new(type_.to_expression(
                        Expression::Variable {
                            name: type_var_name,
                        },
                        body,
                    ))),
                })
            }
            Pattern::Variable { name, type_: None } => Expression::Block(Block {
                statements: vec![Statement::Let {
                    name: name.clone(),
                    mutable: false,
                    expr: target_var.clone(),
                }],
                expr: Some(Box::new(body)),
            }),
            Pattern::Literal(v) => Expression::If {
                condition: Box::new(Expression::Op {
                    name: intern("__eq"),
                    args: vec![target_var, Expression::Literal { value: v }],
                }),
                then: Box::new(body),
                else_: None,
            },
            Pattern::Vec { values, allow_tail } => {
                let len = values.len();
                let mut expr = body;

                for (i, pat) in values.into_iter().enumerate().rev() {
                    expr = pat.to_expression(
                        Expression::Op {
                            name: intern("__index"),
                            args: vec![
                                target_var.clone(),
                                Expression::Literal {
                                    value: Literal::Int(i as i64),
                                },
                            ],
                        },
                        expr,
                    );
                }

                // Check length
                if !allow_tail {
                    expr = Expression::If {
                        condition: Box::new(Expression::Op {
                            name: intern("__eq"),
                            args: vec![
                                Expression::Op {
                                    name: intern("__index"),
                                    args: vec![
                                        target_var.clone(),
                                        Expression::Literal {
                                            value: Literal::String(intern("len")),
                                        },
                                    ],
                                },
                                Expression::Literal {
                                    value: Literal::Int(len as i64),
                                },
                            ],
                        }),
                        then: Box::new(expr),
                        else_: None,
                    };
                }

                // Check type
                Expression::If {
                    condition: Box::new(Expression::Op {
                        name: intern("__eq"),
                        args: vec![
                            Expression::Call {
                                callee: Box::new(Expression::StaticNativeFn {
                                    native_fn: &NF_TYPEOF,
                                }),
                                args: vec![VecAppend::Element(target_var.clone())],
                            },
                            Expression::Literal {
                                value: Literal::String(intern("vec")),
                            },
                        ],
                    }),
                    then: Box::new(expr),
                    else_: None,
                }
            }
            Pattern::Dict(fields) => {
                let mut expr = body;

                for (key, pat) in fields.into_iter().rev() {
                    expr = pat.to_expression(
                        Expression::Op {
                            name: intern("__index"),
                            args: vec![
                                target_var.clone(),
                                Expression::Literal {
                                    value: Literal::String(key),
                                },
                            ],
                        },
                        expr,
                    );
                }

                Expression::If {
                    condition: Box::new(Expression::Op {
                        name: intern("__eq"),
                        args: vec![
                            Expression::Call {
                                callee: Box::new(Expression::StaticNativeFn {
                                    native_fn: &NF_TYPEOF,
                                }),
                                args: vec![VecAppend::Element(target_var.clone())],
                            },
                            Expression::Literal {
                                value: Literal::String(intern("dict")),
                            },
                        ],
                    }),
                    then: Box::new(expr),
                    else_: None,
                }
            }
            Pattern::Struct {
                constructor,
                fields,
            } => {
                let mut expr = body;

                for (key, pat) in fields.into_iter().rev() {
                    expr = pat.to_expression(
                        Expression::Op {
                            name: intern("__index"),
                            args: vec![
                                target_var.clone(),
                                Expression::Literal {
                                    value: Literal::String(key),
                                },
                            ],
                        },
                        expr,
                    );
                }

                Expression::If {
                    condition: Box::new(Expression::Op {
                        name: intern("__eq"),
                        args: vec![
                            Expression::Call {
                                callee: Box::new(Expression::StaticNativeFn {
                                    native_fn: &NF_STRUCT_TYPE,
                                }),
                                args: vec![VecAppend::Element(target_var.clone())],
                            },
                            Expression::Variable { name: constructor },
                        ],
                    }),
                    then: Box::new(expr),
                    else_: None,
                }
            }
        }
    }
}

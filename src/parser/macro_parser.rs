use crate::{
    parser::{
        identifier, keyword,
        lexer::{AsStr, Token},
        op,
    },
    string::Str,
};

use nom::{
    branch::alt,
    combinator::{cut, fail, map, value},
    multi::many0,
    IResult,
};

use super::macros::*;

pub fn macro_def<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], Macro> {
    let (i, _) = keyword("macro")(i)?;
    let (i, _) = op("!")(i)?;
    cut(|i| {
        let (i, name) = identifier(i)?;
        let (i, _) = paren('{')(i)?;
        let (i, rules) = many0(macro_rule)(i)?;
        let (i, _) = paren('}')(i)?;
        Ok((i, Macro::Rules(MacroRules { name, rules })))
    })(i)
}

pub fn macro_call<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], MacroCall> {
    let (i, name) = identifier(i)?;
    let (i, _) = op("!")(i)?;
    let (i, token) = token_item(i)?;
    Ok((i, MacroCall { name, arg: token }))
}

fn macro_rule<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], MacroRule> {
    let (i, pattern) = macro_pattern(i)?;
    let (i, _) = op("=>")(i)?;
    let (i, template) = token_item(i)?;
    Ok((i, MacroRule::new(pattern, template)))
}

fn macro_pattern<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], Pattern> {
    alt((
        // |i| {
        //     let (i, _) = tag("$")(i)?;
        //     let (i, _) = sp(i)?;
        //     let (i, variable) = identifier(i)?;
        //     let (i, _) = sp(i)?;
        //     let (i, _) = tag(":")(i)?;
        //     let (i, _) = sp(i)?;
        //     let (i, item) = macro_pattern_item(i)?;
        //     Ok((
        //         i,
        //         Pattern {
        //             variable: Some(variable),
        //             item,
        //         },
        //     ))
        // },
        |i| {
            let (i, _) = op("$")(i)?;
            let (i, variable) = identifier(i)?;
            Ok((
                i,
                Pattern {
                    variable: Some(variable),
                    item: PatternItem::Any,
                },
            ))
        },
        map(macro_pattern_item, |item| Pattern {
            variable: None,
            item,
        }),
    ))(i)
}

fn macro_pattern_item<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], PatternItem> {
    alt((
        |i| {
            let (i, paren_type) = alt((paren('('), paren('['), paren('{')))(i)?;
            let (i, appends) = many0(macro_pattern_seq_append)(i)?;
            let (i, _) = paren(match paren_type {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => unreachable!(),
            })(i)?;
            Ok((
                i,
                PatternItem::Paren {
                    paren_type,
                    appends,
                },
            ))
        },
        map(any_op, |op| PatternItem::Token(op)),
        |i: &'a [Token<S>]| {
            if let Some(t) = i.first() {
                match t {
                    Token::Keyword(_)
                    | Token::Identifier(_)
                    | Token::String(_)
                    | Token::Int(_)
                    | Token::Float(_) => Ok((&i[1..], PatternItem::Token(t.interned()))),
                    Token::Operator(_) | Token::Paren(_) => fail(i),
                }
            } else {
                fail(i)
            }
        },
    ))(i)
}

fn macro_pattern_seq_append<'a, S: AsStr>(
    i: &'a [Token<S>],
) -> IResult<&'a [Token<S>], PatternSeqAppend> {
    alt((
        |i| {
            let (i, _) = op("$")(i)?;
            let (i, _) = paren('(')(i)?;
            let (i, seq) = many0(macro_pattern)(i)?;
            let (i, _) = paren(')')(i)?;
            let (i, type_) = alt((
                value(RepeatType::ZeroOrMore, op("*")),
                value(RepeatType::OneOrMore, op("+")),
                value(RepeatType::ZeroOrOne, op("?")),
            ))(i)?;
            Ok((i, PatternSeqAppend::Repeat { type_, seq }))
        },
        map(macro_pattern, PatternSeqAppend::Pattern),
    ))(i)
}

fn any_op<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], Token<Str>> {
    map(
        alt((
            alt((
                op("=="),
                op("!="),
                op(">="),
                op("<="),
                op("||"),
                op("&&"),
                op(".."),
                op("=>"),
            )),
            alt((
                op("="),
                op("+"),
                op("-"),
                op("*"),
                op("/"),
                op("%"),
                op("!"),
                op(">"),
                op("<"),
                op("."),
                op(","),
                op(":"),
                op(";"),
                op("|"),
                op("'"),
            )),
        )),
        |s: &S| Token::Operator(s.interned()),
    )(i)
}

fn token_item<'a, S: AsStr>(i: &'a [Token<S>]) -> IResult<&'a [Token<S>], TokenItem> {
    alt((
        |i| {
            let (i, paren_type) = alt((paren('('), paren('['), paren('{')))(i)?;
            let (i, appends) = many0(alt((map(token_item, TokenSeqAppend::Token), |i| {
                let (i, _) = op("$")(i)?;
                let (i, _) = paren('(')(i)?;
                let (i, tokens) = many0(token_item)(i)?;
                let (i, _) = paren(')')(i)?;
                Ok((i, TokenSeqAppend::Repeat(tokens)))
            })))(i)?;
            let (i, _) = paren(match paren_type {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => unreachable!(),
            })(i)?;
            Ok((i, TokenItem::Paren(paren_type, appends)))
        },
        |i| {
            let (i, _) = op("$")(i)?;
            let (i, var) = identifier(i)?;
            Ok((i, TokenItem::Variable(var)))
        },
        map(any_op, |op| TokenItem::Token(op)),
        |i: &'a [Token<S>]| {
            if let Some(t) = i.first() {
                match t {
                    Token::Keyword(_)
                    | Token::Identifier(_)
                    | Token::String(_)
                    | Token::Int(_)
                    | Token::Float(_) => Ok((&i[1..], TokenItem::Token(t.interned()))),
                    Token::Operator(_) | Token::Paren(_) => fail(i),
                }
            } else {
                fail(i)
            }
        },
    ))(i)
}

fn paren<S: AsStr>(name: char) -> impl FnMut(&[Token<S>]) -> IResult<&[Token<S>], char> {
    move |i| {
        if let Some(Token::Paren(c)) = i.first() {
            if *c == name {
                return Ok((&i[1..], name));
            }
        }
        fail(i)
    }
}

use std::sync::Arc;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, digit1, one_of},
    combinator::{map, not, opt, recognize, value},
    multi::{many0_count, many1},
    number::complete::double,
    sequence::{pair, terminated},
    IResult,
};

use crate::string::{intern, Str};

use super::string::parse_string;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: AsStr> {
    Keyword(S),
    Identifier(S),
    Operator(S),
    Paren(char),
    String(Str),
    Int(i64),
    Float(f64),
}

pub fn lex(mut i: &str) -> IResult<&str, Vec<Token<&str>>> {
    let mut tokens = vec![];

    while !i.is_empty() {
        let (i_, _) = sp(i).unwrap();
        let Ok((i_, token)) = alt((
            paren,
            operator,
            map(keyword, Token::Keyword),
            map(identifier, Token::Identifier),
            map(parse_string, |s| Token::String(Arc::new(s))),
            map(terminated(digit1, not(char('.'))), |s: &str| {
                Token::Int(s.parse().unwrap())
            }),
            map(double, Token::Float),
        ))(i_) else {
            break;
        };
        i = i_;
        tokens.push(token);
    }
    let (i, _) = sp(i).unwrap();

    Ok((i, tokens))
}

fn paren(i: &str) -> IResult<&str, Token<&str>> {
    map(one_of("(){}[]"), Token::Paren)(i)
}

fn operator(i: &str) -> IResult<&str, Token<&str>> {
    map(
        alt((
            alt((
                tag("=="),
                tag("!="),
                tag(">="),
                tag("<="),
                tag("||"),
                tag("&&"),
                tag(".."),
                tag("=>"),
            )),
            alt((
                tag("="),
                tag("+"),
                tag("-"),
                tag("*"),
                tag("/"),
                tag("%"),
                tag("!"),
                tag(">"),
                tag("<"),
                tag("."),
                tag(","),
                tag(":"),
                tag(";"),
                tag("|"),
                tag("'"),
                tag("$"),
            )),
        )),
        Token::Operator,
    )(i)
}

fn keyword(i: &str) -> IResult<&str, &str> {
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
            tag("macro"),
            tag("mod"),
            tag("true"),
            tag("false"),
            tag("null"),
        )),
        not(alt((alphanumeric1, tag("_")))),
    )(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    let (i, _) = not(keyword)(i)?;

    let (i, str) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(i)?;
    Ok((i, str))
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

impl<S: AsStr> Token<S> {
    pub fn interned(&self) -> Token<Str> {
        match self {
            Token::Keyword(s) => Token::Keyword(s.interned()),
            Token::Identifier(s) => Token::Identifier(s.interned()),
            Token::Operator(s) => Token::Operator(s.interned()),
            Token::Paren(c) => Token::Paren(*c),
            Token::String(s) => Token::String(s.clone()),
            Token::Int(i) => Token::Int(*i),
            Token::Float(f) => Token::Float(*f),
        }
    }
}

pub trait AsStr {
    fn as_str(&self) -> &str;
    fn interned(&self) -> Str;
}

impl AsStr for &str {
    fn as_str(&self) -> &str {
        *self
    }

    fn interned(&self) -> Str {
        intern(*self)
    }
}

impl AsStr for Str {
    fn as_str(&self) -> &str {
        self.as_ref()
    }

    fn interned(&self) -> Str {
        self.clone()
    }
}

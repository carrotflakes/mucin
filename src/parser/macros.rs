use crate::string::Str;

use super::lexer::Token;

#[derive(Debug, Clone)]
pub enum Macro {
    Rules(MacroRules),
    NativeFn {
        name: Str,
        fun: fn(TokenItem) -> Vec<Token<Str>>,
    },
}

#[derive(Debug, Clone)]
pub struct MacroRules {
    pub name: Str,
    pub rules: Vec<MacroRule>,
}

#[derive(Debug, Clone)]
pub struct MacroRule {
    pub pattern: Pattern,
    pub template: TokenItem,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub variable: Option<Str>,
    pub item: PatternItem,
}

#[derive(Debug, Clone)]
pub enum PatternItem {
    Any,
    Token(Token<Str>),
    Paren {
        paren_type: char,
        appends: Vec<PatternSeqAppend>,
    },
}

#[derive(Debug, Clone)]
pub enum PatternSeqAppend {
    Pattern(Pattern),
    Repeat {
        type_: RepeatType,
        seq: Vec<Pattern>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RepeatType {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

#[derive(Debug, Clone)]
pub struct MacroCall {
    pub name: Str,
    pub arg: TokenItem,
}

#[derive(Debug, Clone)]
pub enum TokenItem {
    Variable(Str),
    Token(Token<Str>),
    Paren(char, Vec<TokenSeqAppend>),
}

#[derive(Debug, Clone)]
pub enum TokenSeqAppend {
    Token(TokenItem),
    Repeat(Vec<TokenItem>),
}

#[derive(Debug, Clone)]
pub enum CapturedToken {
    Token(TokenItem),
    Vec(Vec<Option<CapturedToken>>),
}

impl CapturedToken {
    pub fn depth(&self) -> usize {
        match self {
            CapturedToken::Token(_) => 0,
            CapturedToken::Vec(v) => {
                let mut max = 0;
                for t in v {
                    if let Some(t) = t {
                        max = max.max(t.depth());
                    }
                }
                max + 1
            }
        }
    }

    fn get(&self, indexes: &[usize]) -> Option<TokenItem> {
        match self {
            CapturedToken::Token(token) => {
                if indexes.is_empty() {
                    Some(token.clone())
                } else {
                    None
                }
            }
            CapturedToken::Vec(v) => {
                if let Some((index, indexes)) = indexes.split_first() {
                    if let Some(ct) = v.get(*index) {
                        ct.as_ref().and_then(|ct| ct.get(indexes))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
}

impl Macro {
    pub fn name(&self) -> Str {
        match self {
            Macro::Rules(rules) => rules.name.clone(),
            Macro::NativeFn { name, .. } => name.clone(),
        }
    }

    pub fn expand<'a>(&self, token: &TokenItem) -> Option<Vec<Token<Str>>> {
        match self {
            Macro::Rules(rules) => rules.expand(token),
            Macro::NativeFn { fun, .. } => Some(fun(token.clone())),
        }
    }
}

impl MacroRules {
    pub fn expand<'a>(&self, token: &TokenItem) -> Option<Vec<Token<Str>>> {
        for rule in &self.rules {
            let mut bindings = vec![];
            if macro_pattern_match(&mut bindings, &rule.pattern, token) {
                let mut tokens = Vec::new();
                rule.template.render(&bindings, &mut vec![], &mut tokens);
                return Some(tokens);
            }
        }
        None
    }
}

pub fn macro_pattern_match(
    bindings: &mut Vec<(Str, CapturedToken)>,
    pattern: &Pattern,
    token: &TokenItem,
) -> bool {
    if let Some(var) = &pattern.variable {
        bindings.push((var.clone(), CapturedToken::Token(token.clone())));
    }

    match (&pattern.item, token) {
        (PatternItem::Any, _) => true,
        (PatternItem::Token(t1), TokenItem::Token(t2)) => t1 == t2,
        (
            PatternItem::Paren {
                paren_type,
                appends,
            },
            TokenItem::Paren(paren_type2, tokens),
        ) if paren_type == paren_type2 => {
            let mut i = 0;
            for append in appends {
                match append {
                    PatternSeqAppend::Pattern(pat) => {
                        let Some(TokenSeqAppend::Token(token)) = tokens.get(i) else {
                            return false;
                        };
                        if !macro_pattern_match(bindings, pat, token) {
                            return false;
                        }
                        i += 1;
                    }
                    PatternSeqAppend::Repeat { type_, seq } => {
                        let max = match type_ {
                            RepeatType::ZeroOrMore => usize::MAX,
                            RepeatType::OneOrMore => usize::MAX,
                            RepeatType::ZeroOrOne => 1,
                        };
                        let mut j = i;
                        let mut bss: Vec<Vec<(Str, CapturedToken)>> = vec![];
                        'outer: while j < max {
                            bss.push(vec![]);
                            for pat in seq {
                                let Some(TokenSeqAppend::Token(token)) = tokens.get(i) else {
                                    bss.pop();
                                    break 'outer;
                                };
                                if !macro_pattern_match(bss.last_mut().unwrap(), pat, token) {
                                    bss.pop();
                                    break 'outer;
                                }
                                i += 1;
                            }
                            j = i;
                        }
                        i = j;

                        let mut names: Vec<std::sync::Arc<String>> = vec![];
                        for bs in &bss {
                            for (name, _) in bs {
                                if !names.contains(&name) {
                                    names.push(name.clone());
                                }
                            }
                        }

                        bindings.extend(
                            names
                                .iter()
                                .map(|name| {
                                    (
                                        name.clone(),
                                        CapturedToken::Vec(
                                            bss.iter()
                                                .map(|bs| {
                                                    bs.iter()
                                                        .find(|(name2, _)| name2 == name)
                                                        .map(|(_, token)| token.clone())
                                                })
                                                .collect(),
                                        ),
                                    )
                                })
                                .collect::<Vec<_>>(),
                        );

                        if matches!(type_, RepeatType::OneOrMore) && bss.is_empty() {
                            return false;
                        }
                    }
                }
            }
            tokens.len() == i
        }
        _ => false,
    }
}

impl TokenItem {
    pub fn render<'a>(
        &self,
        bindings: &[(Str, CapturedToken)],
        indexes: &mut Vec<usize>,
        rendered: &mut Vec<Token<Str>>,
    ) -> bool {
        match self {
            TokenItem::Variable(var) => {
                if let Some(token) = bindings
                    .iter()
                    .find(|(v, _)| v == var)
                    .and_then(|(_, ct)| ct.get(indexes))
                {
                    token.render(bindings, indexes, rendered); // TODO:?
                } else {
                    // dbg!(bindings, indexes, var);
                    return false;
                }
            }
            TokenItem::Token(token) => rendered.push(token.clone()),
            TokenItem::Paren(paren_type, tokens) => {
                rendered.push(Token::Paren(*paren_type));

                for ta in tokens {
                    match ta {
                        TokenSeqAppend::Token(t) => {
                            if !t.render(bindings, indexes, rendered) {
                                return false;
                            }
                        }
                        TokenSeqAppend::Repeat(ts) => {
                            indexes.push(0);
                            'outer: loop {
                                let string_len = rendered.len();
                                for t in ts.iter() {
                                    if !t.render(bindings, indexes, rendered) {
                                        rendered.truncate(string_len);
                                        break 'outer;
                                    }
                                }
                                *indexes.last_mut().unwrap() += 1;
                            }
                            indexes.pop();
                        }
                    }
                }

                rendered.push(Token::Paren(match paren_type {
                    '(' => ')',
                    '[' => ']',
                    '{' => '}',
                    _ => unreachable!(),
                }));
            }
        }
        true
    }
}

impl std::fmt::Display for TokenItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenItem::Variable(_) => panic!(),
            TokenItem::Token(token) => match token {
                Token::Keyword(s) => write!(f, "{}", s),
                Token::Identifier(s) => write!(f, "{}", s),
                Token::Operator(s) => write!(f, "{}", s),
                Token::Paren(s) => write!(f, "{}", s),
                Token::String(s) => write!(f, "{}", s),
                Token::Int(c) => write!(f, "{}", c),
                Token::Float(c) => write!(f, "{}", c),
            },
            TokenItem::Paren(paren_type, tokens) => {
                write!(f, "{}", paren_type)?;
                for t in tokens {
                    match t {
                        TokenSeqAppend::Token(t) => write!(f, "{} ", t)?,
                        TokenSeqAppend::Repeat(_) => unreachable!(),
                    }
                }
                write!(
                    f,
                    "{}",
                    match paren_type {
                        '(' => ')',
                        '[' => ']',
                        '{' => '}',
                        _ => unreachable!(),
                    }
                )
            }
        }
    }
}

pub mod parser {

    use crate::parser::{
        identifier, keyword,
        lexer::{AsStr, Token},
        op,
    };
    use nom::{
        branch::alt,
        combinator::{cut, fail, map, value},
        multi::many0,
        IResult,
    };

    use super::*;

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
        Ok((i, MacroRule { pattern, template }))
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
}

pub fn builtin() -> Vec<Macro> {
    vec![Macro::NativeFn {
        name: crate::string::intern("stringify"),
        fun: |token| {
            let mut tokens = Vec::new();
            token.render(&[], &mut vec![], &mut tokens);
            tokens
        },
    }]
}

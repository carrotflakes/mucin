use crate::string::Str;

#[derive(Debug, Clone)]
pub enum Macro {
    Rules(MacroRules),
    NativeFn {
        name: Str,
        fun: fn(TokenItem) -> String,
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
    Op(String),
    Ident(Str),
    Literal(Str),
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
pub enum TokenItem {
    Variable(Str),
    Op(String),
    Ident(Str),
    Literal(Str),
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

    pub fn expand(&self, token: &TokenItem) -> Option<String> {
        match self {
            Macro::Rules(rules) => rules.expand(token),
            Macro::NativeFn { fun, .. } => Some(fun(token.clone())),
        }
    }
}

impl MacroRules {
    pub fn expand(&self, token: &TokenItem) -> Option<String> {
        for rule in &self.rules {
            let mut bindings = vec![];
            if macro_pattern_match(&mut bindings, &rule.pattern, token) {
                let mut string = String::new();
                rule.template.render(&bindings, &mut vec![], &mut string);
                return Some(string);
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
        (PatternItem::Op(op), TokenItem::Op(op2)) => op == op2,
        (PatternItem::Ident(id), TokenItem::Ident(id2)) => id == id2,
        (PatternItem::Literal(lit), TokenItem::Literal(lit2)) => lit == lit2,
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
    pub fn render(
        &self,
        bindings: &[(Str, CapturedToken)],
        indexes: &mut Vec<usize>,
        string: &mut String,
    ) -> bool {
        match self {
            TokenItem::Variable(var) => {
                if let Some(token) = bindings
                    .iter()
                    .find(|(v, _)| v == var)
                    .and_then(|(_, ct)| ct.get(indexes))
                {
                    string.push_str(&format!("{}", token));
                } else {
                    // dbg!(bindings, indexes, var);
                    return false;
                }
            }
            TokenItem::Op(op) => string.push_str(op),
            TokenItem::Ident(id) => string.push_str(id),
            TokenItem::Literal(lit) => string.push_str(&format!("{}", lit)),
            TokenItem::Paren(paren_type, tokens) => {
                string.push(*paren_type);

                for ta in tokens {
                    match ta {
                        TokenSeqAppend::Token(t) => {
                            if !t.render(bindings, indexes, string) {
                                return false;
                            }
                        }
                        TokenSeqAppend::Repeat(ts) => {
                            indexes.push(0);
                            'outer: loop {
                                let string_len = string.len();
                                for t in ts.iter() {
                                    if !t.render(bindings, indexes, string) {
                                        string.truncate(string_len);
                                        break 'outer;
                                    }
                                }
                                *indexes.last_mut().unwrap() += 1;
                            }
                            indexes.pop();
                        }
                    }
                }

                string.push(match paren_type {
                    '(' => ')',
                    '[' => ']',
                    '{' => '}',
                    _ => unreachable!(),
                });
            }
        }
        string.push_str(" ");
        true
    }
}

impl std::fmt::Display for TokenItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenItem::Variable(_) => panic!(),
            TokenItem::Op(op) => write!(f, "{}", op),
            TokenItem::Ident(ident) => write!(f, "{}", ident),
            TokenItem::Literal(lit) => write!(f, "{}", lit),
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
    use std::sync::Arc;

    use crate::{
        parser::{identifier, literal, sp},
        string::intern,
    };
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alpha1, alphanumeric1, char, one_of},
        combinator::{cut, map, not, recognize, value},
        multi::{many0, many0_count},
        sequence::{pair, preceded},
        IResult,
    };

    use super::*;

    pub fn macro_def(i: &str) -> IResult<&str, MacroRules> {
        let (i, _) = tag("macro")(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("!")(i)?;
        let (i, _) = sp(i)?;
        cut(|i| {
            let (i, name) = identifier(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("{")(i)?;
            let (i, _) = sp(i)?;
            let (i, rules) = many0(preceded(sp, macro_rule))(i)?;
            let (i, _) = sp(i)?;
            let (i, _) = tag("}")(i)?;
            Ok((
                i,
                MacroRules {
                    name: intern(name),
                    rules,
                },
            ))
        })(i)
    }

    pub fn macro_call(i: &str) -> IResult<&str, (Str, TokenItem)> {
        let (i, name) = identifier(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("!")(i)?;
        let (i, _) = not(char('='))(i)?;
        let (i, _) = sp(i)?;
        let (i, token) = token_item(i)?;
        Ok((i, (intern(name), token)))
    }

    fn macro_rule(i: &str) -> IResult<&str, MacroRule> {
        let (i, pattern) = macro_pattern(i)?;
        let (i, _) = sp(i)?;
        let (i, _) = tag("=>")(i)?;
        let (i, _) = sp(i)?;
        let (i, template) = token_item(i)?;
        Ok((i, MacroRule { pattern, template }))
    }

    fn macro_pattern(i: &str) -> IResult<&str, Pattern> {
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
                let (i, _) = tag("$")(i)?;
                let (i, _) = sp(i)?;
                let (i, variable) = identifier(i)?;
                Ok((
                    i,
                    Pattern {
                        variable: Some(intern(variable)),
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

    fn macro_pattern_item(i: &str) -> IResult<&str, PatternItem> {
        alt((
            |i| {
                let (i, paren_type) = one_of("([{")(i)?;
                let (i, appends) = many0(preceded(sp, macro_pattern_seq_append))(i)?;
                let (i, _) = sp(i)?;
                let (i, _) = tag(match paren_type {
                    '(' => ")",
                    '[' => "]",
                    '{' => "}",
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
            map(op, |op| PatternItem::Op(op.to_string())),
            map(recognize(literal), |value| {
                PatternItem::Literal(Arc::new(value.to_string()))
            }),
            map(identifier, |ident| PatternItem::Ident(intern(ident))),
        ))(i)
    }

    fn macro_pattern_seq_append(i: &str) -> IResult<&str, PatternSeqAppend> {
        alt((
            |i| {
                let (i, _) = tag("$")(i)?;
                let (i, _) = sp(i)?;
                let (i, _) = tag("(")(i)?;
                let (i, seq) = many0(preceded(sp, macro_pattern))(i)?;
                let (i, _) = sp(i)?;
                let (i, _) = tag(")")(i)?;
                let (i, _) = sp(i)?;
                let (i, type_) = alt((
                    value(RepeatType::ZeroOrMore, tag("*")),
                    value(RepeatType::OneOrMore, tag("+")),
                    value(RepeatType::ZeroOrOne, tag("?")),
                ))(i)?;
                let (i, _) = sp(i)?;
                Ok((i, PatternSeqAppend::Repeat { type_, seq }))
            },
            map(macro_pattern, PatternSeqAppend::Pattern),
        ))(i)
    }

    fn op(i: &str) -> IResult<&str, &str> {
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
                tag("_"),
            )),
        ))(i)
    }

    fn token_item(i: &str) -> IResult<&str, TokenItem> {
        alt((
            |i| {
                let (i, paren_type) = one_of("([{")(i)?;
                let (i, appends) = many0(preceded(
                    sp,
                    alt((map(token_item, TokenSeqAppend::Token), |i| {
                        let (i, _) = tag("$")(i)?;
                        let (i, _) = sp(i)?;
                        let (i, _) = tag("(")(i)?;
                        let (i, tokens) = many0(preceded(sp, token_item))(i)?;
                        let (i, _) = sp(i)?;
                        let (i, _) = tag(")")(i)?;
                        Ok((i, TokenSeqAppend::Repeat(tokens)))
                    })),
                ))(i)?;
                let (i, _) = sp(i)?;
                let (i, _) = tag(match paren_type {
                    '(' => ")",
                    '[' => "]",
                    '{' => "}",
                    _ => unreachable!(),
                })(i)?;
                Ok((i, TokenItem::Paren(paren_type, appends)))
            },
            map(recognize(literal), |value| {
                TokenItem::Literal(Arc::new(value.to_string()))
            }),
            map(ident, TokenItem::Ident),
            |i| {
                let (i, _) = tag("$")(i)?;
                let (i, _) = sp(i)?;
                let (i, var) = identifier(i)?;
                Ok((i, TokenItem::Variable(intern(var))))
            },
            map(op, |op| TokenItem::Op(op.to_string())),
        ))(i)
    }

    fn ident(i: &str) -> IResult<&str, Str> {
        let (i, str) = recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        ))(i)?;
        Ok((i, Arc::new(str.to_string())))
    }
}

pub fn builtin() -> Vec<Macro> {
    vec![Macro::NativeFn {
        name: crate::string::intern("stringify"),
        fun: |token| {
            let mut string = String::new();
            token.render(&[], &mut vec![], &mut string);
            format!("{:?}", string)
        },
    }]
}

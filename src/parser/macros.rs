use crate::string::{intern, Str};

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
    pub pattern_variables: Vec<(Str, usize)>,
    pub template_variables: Vec<(Str, usize)>,
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
    // Variable will be only used in template
    Variable(Str),
    Token(Token<Str>),
    Paren(char, Vec<TokenSeqAppend>),
}

#[derive(Debug, Clone)]
pub enum TokenSeqAppend {
    Token(TokenItem),
    // Repeat will be only used in template
    Repeat(Vec<TokenItem>),
}

pub struct Bind {
    name: Str,
    bind: Result<CapturedToken, usize>,
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
            let mut bindings = rule
                .pattern_variables
                .iter()
                .map(|(name, depth)| Bind {
                    name: name.clone(),
                    bind: Ok(CapturedToken::new(*depth)),
                })
                .collect();
            if rule
                .pattern
                .pattern_match(&mut bindings, &mut vec![], token)
            {
                for v in &rule.template_variables {
                    if !bindings.iter().any(|b| &b.name == &v.0) {
                        bindings.push(Bind {
                            name: v.0.clone(),
                            bind: Err(v.1),
                        });
                    }
                }
                let mut tokens = Vec::new();
                if !rule
                    .template
                    .render(&mut bindings, &mut vec![], &mut tokens)
                {
                    panic!("render failed: {:?}", tokens);
                }
                // println!("tokens: {:?}", &tokens);
                return Some(tokens);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub enum CapturedToken {
    Token(Option<TokenItem>),
    Vec(Vec<CapturedToken>),
}

impl CapturedToken {
    pub fn new(depth: usize) -> Self {
        if depth == 0 {
            CapturedToken::Token(None)
        } else {
            CapturedToken::Vec(vec![CapturedToken::new(depth - 1)])
        }
    }

    fn get(&self, indexes: &[usize]) -> Option<TokenItem> {
        match self {
            CapturedToken::Token(token) => token.clone(),
            CapturedToken::Vec(v) => {
                if let Some((index, indexes)) = indexes.split_first() {
                    if let Some(ct) = v.get(*index) {
                        ct.get(indexes)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn set(&mut self, indexes: &[usize], token: TokenItem) -> bool {
        match self {
            CapturedToken::Token(t) => {
                if let Some(t) = t {
                    t.unify(&token)
                } else {
                    *t = Some(token);
                    true
                }
            }
            CapturedToken::Vec(v) => {
                if let Some((index, indexes)) = indexes.split_first() {
                    v.resize_with(*index + 1, || CapturedToken::new(indexes.len()));
                    if let Some(ct) = v.get_mut(*index) {
                        ct.set(indexes, token)
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        }
    }
}

impl MacroRule {
    pub fn new(pattern: Pattern, template: TokenItem) -> Self {
        let mut pattern_variables = vec![];
        pattern.collect_variables(0, &mut pattern_variables);
        let mut template_variables = vec![];
        template.collect_variables(0, &mut template_variables);

        Self {
            pattern,
            template,
            pattern_variables,
            template_variables,
        }
    }
}

impl Pattern {
    pub fn collect_variables(&self, depth: usize, variables: &mut Vec<(Str, usize)>) {
        if let Some(var) = &self.variable {
            if let Some((_, d)) = variables.iter_mut().find(|(v, _)| v == var) {
                *d = depth.min(*d);
            } else {
                variables.push((var.clone(), depth));
            }
        }

        match &self.item {
            PatternItem::Any => {}
            PatternItem::Token(_) => {}
            PatternItem::Paren { appends, .. } => {
                for append in appends {
                    match append {
                        PatternSeqAppend::Pattern(pat) => pat.collect_variables(depth, variables),
                        PatternSeqAppend::Repeat { seq, .. } => {
                            for pat in seq {
                                pat.collect_variables(depth + 1, variables);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn pattern_match(
        &self,
        bindings: &mut Vec<Bind>,
        indexes: &mut Vec<usize>,
        token: &TokenItem,
    ) -> bool {
        if let Some(var) = &self.variable {
            let bind = bindings
                .iter_mut()
                .find(|b| &b.name == var)
                .expect("binding not found");
            if !bind.bind.as_mut().unwrap().set(indexes, token.clone()) {
                return false;
            }
        }

        match (&self.item, token) {
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
                            if !pat.pattern_match(bindings, indexes, token) {
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
                            let mut k = 0;
                            'outer: while k < max {
                                indexes.push(k);
                                for pat in seq {
                                    let Some(TokenSeqAppend::Token(token)) = tokens.get(i) else {
                                        indexes.pop();
                                        break 'outer;
                                    };
                                    if !pat.pattern_match(bindings, indexes, token) {
                                        indexes.pop();
                                        break 'outer;
                                    }
                                    i += 1;
                                }
                                indexes.pop();
                                j = i;
                                k += 1;
                            }
                            i = j;

                            if matches!(type_, RepeatType::OneOrMore) && k == 0 {
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
}

impl TokenItem {
    pub fn collect_variables(&self, depth: usize, variables: &mut Vec<(Str, usize)>) {
        match self {
            TokenItem::Variable(var) => {
                if let Some((_, d)) = variables.iter_mut().find(|(v, _)| v == var) {
                    *d = depth.min(*d);
                } else {
                    variables.push((var.clone(), depth));
                }
            }
            TokenItem::Token(_) => {}
            TokenItem::Paren(_, appends) => {
                for append in appends {
                    match append {
                        TokenSeqAppend::Token(ti) => ti.collect_variables(depth, variables),
                        TokenSeqAppend::Repeat(tis) => {
                            for ti in tis {
                                ti.collect_variables(depth + 1, variables);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn render(
        &self,
        bindings: &mut Vec<Bind>,
        indexes: &mut Vec<usize>,
        rendered: &mut Vec<Token<Str>>,
    ) -> bool {
        match self {
            TokenItem::Variable(var) => {
                let bind = bindings.iter_mut().find(|b| &b.name == var).unwrap();
                match &mut bind.bind {
                    Ok(cap) => {
                        if let Some(token) = cap.get(indexes) {
                            token.render(bindings, indexes, rendered);
                        } else {
                            // End of sequence
                            return false;
                        }
                    }
                    Err(depth) => {
                        let v = intern(&indexes[..*depth].iter().map(|i| i.to_string()).fold(
                            format!("#${}", var),
                            |mut acc, cur| {
                                acc.push_str("-");
                                acc.push_str(&cur);
                                acc
                            },
                        ));
                        rendered.push(Token::Identifier(v));
                    }
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
                        TokenSeqAppend::Repeat(tis) => {
                            indexes.push(0);
                            'outer: loop {
                                let string_len = rendered.len();
                                for ti in tis.iter() {
                                    if !ti.render(bindings, indexes, rendered) {
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

    pub fn unify(&mut self, other: &Self) -> bool {
        match (self, other) {
            (TokenItem::Variable(_), _) => panic!(),
            (TokenItem::Token(token1), TokenItem::Token(token2)) => token1 == token2,
            (TokenItem::Paren(paren_type1, tokens1), TokenItem::Paren(paren_type2, tokens2)) => {
                if paren_type1 != paren_type2 {
                    return false;
                }
                if tokens1.len() != tokens2.len() {
                    return false;
                }
                for (t1, t2) in tokens1.iter_mut().zip(tokens2) {
                    match (t1, t2) {
                        (TokenSeqAppend::Token(t1), TokenSeqAppend::Token(t2)) => {
                            if !t1.unify(t2) {
                                return false;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                true
            }
            _ => false,
        }
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

pub fn builtin() -> Vec<Macro> {
    vec![Macro::NativeFn {
        name: crate::string::intern("stringify"),
        fun: |token| {
            let mut tokens = Vec::new();
            token.render(&mut vec![], &mut vec![], &mut tokens);
            tokens
        },
    }]
}

use std::fmt::{ Debug, Display };

use derive_more::{ AsRef, Deref, DerefMut, Display as DeriveDisplay, From, IntoIterator };
use derive_flat_debug::DebugFlat;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Definition(Definition),
    Main(Query),

    // -- Tokenizer intermediate tokens --
    Query(Query),
    QueryToken(QueryToken),
    Filter(Filter),
    FilterToken(FilterToken),
    FilterOperation(FilterOperation),
    FilterOperationToken(FilterOperationToken),
    FilterImplicitToken(ImplicitFilterToken),
    ExtendSuper(Vec<Token>),
    // todo: Lambda(Lambda),
}

#[derive(AsRef, Clone, Debug, Default, Deref, DerefMut, From, IntoIterator, PartialEq)]
pub struct Query(pub Vec<QueryToken>);

#[derive(Clone, DebugFlat, PartialEq)]
pub enum QueryToken {
    #[debug(flatten)] Group(Molecule),
    Molecule(Molecule),
    Atom(Atom),
    #[debug(skip)] Infix(Infix),
    #[debug(skip)] Postfix(Postfix),
    Filter(Filter),
    // todo: Lambda(Lambda),
}

#[derive(AsRef, Clone, Debug, Default, Deref, DerefMut, From, IntoIterator, PartialEq)]
pub struct Molecule(pub Vec<QueryToken>);

#[derive(Clone, DebugFlat, PartialEq)]
pub enum Atom {
    Index(i32),
    Float(Float),
    Ident(String),
    #[debug(skip)] String(String),
    #[debug(flatten)] Lens(LensIdent),
    #[debug(flatten)] Call(LensCall),
    #[debug(skip)] Range(Range),
    Wild(Wild),
}

#[derive(Clone, Debug, From, PartialEq, PartialOrd)]
pub enum Float {
    F32(f32),
    F64(f64),
}

#[derive(AsRef, Clone, Debug, Deref, DerefMut, From, PartialEq, Eq, Hash)]
pub struct LensIdent(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct LensCall {
    pub lens: LensIdent,
    pub args: Vec<Token>,
}

#[derive(Clone, Debug, From, PartialEq, Eq, Hash)]
pub enum Infix {
    Union,
    Intersection,
    Lens(LensIdent),
}

#[derive(Clone, Debug, From, PartialEq, Eq, Hash)]
pub enum Postfix {
    Optional,
    Assertion,
    Lens(LensIdent),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Range {
    Inclusive {
        start: u32,
        end: u32,
    },
    Exclusive {
        start: u32,
        end: u32,
    },
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Wild {
    Once,
    Exact(u32),
    Range(Range),
    Until(Molecule),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Definition {
    pub lens: LensIdent,
    pub params: Vec<LensParam>,
    pub body: LensBody,
}

#[derive(Clone, DebugFlat, DeriveDisplay, From, PartialEq)]
pub enum LensBody {
    Query(Query),
    Filter(Filter),
}

#[derive(Clone, Debug, DeriveDisplay, From, PartialEq, Eq, Hash)]
pub struct LensParam(pub LensIdent);

use filter::*;
pub mod filter {
    use super::*;

    #[derive(AsRef, Clone, Debug, Deref, DerefMut, From, PartialEq)]
    pub struct Filter(pub Vec<FilterToken>);

    #[derive(Clone, DebugFlat, From, PartialEq)]
    pub enum FilterToken {
        Group(Filter),
        Molecule(Molecule),
        #[debug(flatten)] Operation(FilterOperation),
        Postfix(FilterPostfix),
    }

    #[derive(AsRef, Clone, Debug, Deref, DerefMut, From, PartialEq)]
    pub struct FilterOperation(pub Vec<FilterOperationToken>);

    #[derive(Clone, DebugFlat, From, PartialEq)]
    pub enum FilterOperationToken {
        Prefix(FilterPrefix),
        Infix(FilterInfix),
        Postfix(FilterPostfix),
        #[debug(flatten)] Implicit(ImplicitFilterToken),
        Atom(Atom),
        #[debug(flatten)] Group(Filter),
        #[debug(flatten)] Operation(FilterOperation),
    }

    #[derive(Clone, DebugFlat, From, PartialEq)]
    pub enum ImplicitFilterToken {
        #[debug(flatten)] Group(Filter),
        #[debug(flatten)] Lens(LensIdent),
        #[debug(flatten)] Call(LensCall),
        Query(Query),
    }

    #[derive(Clone, Copy, Debug, From, PartialEq, Eq, Hash)]
    pub enum FilterPrefix {
        Not,
        Neg,
    }

    #[derive(Clone, DebugFlat, From, PartialEq, Eq, Hash)]
    pub enum FilterInfix {
        Union,
        Eq,
        Or,
        And,
        Neq,
        Lte,
        Lt,
        Gte,
        Gt,
        Add,
        Sub,
        Mul,
        Div,
        #[debug(flatten)] Lens(LensIdent),
    }

    #[derive(Clone, DebugFlat, From, PartialEq)]
    pub enum FilterPostfix {
        Assertion,
        Filter(Filter),
        /// Called infix but is allowed to behave like a postfix in some contexts.
        #[debug(flatten)]
        InfixLens(LensIdent),
    }
}

use hs::*;
use super::error::TokenError;
pub mod hs {
    use super::*;

    // todo: Data will be enums not strings by the time this is used.

    fn std_finity<'a>() -> Vec<(&'a str, Fixity)> {
        vec![
            ("|", Fixity { assoc: Assoc::Left, precedence: 5 }),
            ("&", Fixity { assoc: Assoc::Left, precedence: 5 }),
            ("+", Fixity { assoc: Assoc::Left, precedence: 6 }),
            ("-", Fixity { assoc: Assoc::Left, precedence: 6 }),
            ("*", Fixity { assoc: Assoc::Left, precedence: 7 }),
            ("/", Fixity { assoc: Assoc::Left, precedence: 7 }),
            ("%", Fixity { assoc: Assoc::Left, precedence: 7 }),
            ("==", Fixity { assoc: Assoc::Left, precedence: 4 }),
            ("!=", Fixity { assoc: Assoc::Left, precedence: 4 }),
            ("<", Fixity { assoc: Assoc::Left, precedence: 4 }),
            (">", Fixity { assoc: Assoc::Left, precedence: 4 }),
            ("<=", Fixity { assoc: Assoc::Left, precedence: 4 }),
            (">=", Fixity { assoc: Assoc::Left, precedence: 4 }),
            ("&&", Fixity { assoc: Assoc::Left, precedence: 3 }),
            ("||", Fixity { assoc: Assoc::Left, precedence: 2 })
        ]
    }

    pub struct Fixity {
        /// Left, Right, or None
        assoc: Assoc,
        precedence: u8,
    }

    pub enum Assoc {
        Left,
        Right,
        None,
    }
}

impl TryFrom<Token> for FilterOperationToken {
    type Error = Token;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::FilterOperationToken(filter_op_token) => { Ok(filter_op_token) }
            Token::QueryToken(QueryToken::Group(group)) => {
                Ok(
                    FilterOperationToken::Implicit(
                        ImplicitFilterToken::Query(group.try_into().unwrap())
                    )
                )
            }
            Token::QueryToken(QueryToken::Atom(Atom::Call(call))) => {
                Ok(FilterOperationToken::Implicit(ImplicitFilterToken::Call(call)))
            }
            Token::QueryToken(QueryToken::Molecule(molecule)) => {
                Ok(FilterOperationToken::Implicit(ImplicitFilterToken::Query(molecule.into())))
            }
            Token::QueryToken(QueryToken::Atom(atom)) => Ok(FilterOperationToken::Atom(atom)),
            Token::Filter(filter) => Ok(FilterOperationToken::Group(filter)),
            Token::FilterOperation(operation) => Ok(FilterOperationToken::Operation(operation)),
            Token::QueryToken(QueryToken::Infix(Infix::Lens(lens))) =>
                Ok(FilterOperationToken::Infix(FilterInfix::Lens(lens))),
            _ => Err(token),
        }
    }
}

impl From<Molecule> for Query {
    fn from(molecule: Molecule) -> Self {
        Query(molecule.0)
    }
}

impl From<FilterOperationToken> for Token {
    fn from(token: FilterOperationToken) -> Self {
        Token::FilterOperationToken(token)
    }
}

impl From<QueryToken> for Token {
    fn from(qt: QueryToken) -> Self {
        match qt {
            QueryToken::Filter(filter) => Token::Filter(filter),
            qt => Token::QueryToken(qt),
        }
    }
}

impl TryFrom<Token> for QueryToken {
    type Error = Token;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::Filter(filter) => Ok(QueryToken::Filter(filter)),
            Token::QueryToken(query_token) => Ok(query_token),
            _ => Err(token),
        }
    }
}

impl TryFrom<Vec<Token>> for Query {
    type Error = Token;

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        match tokens.into_iter().map(QueryToken::try_from).collect::<Result<Vec<_>, _>>() {
            Ok(query) => Ok(query.into()),
            Err(tokens) => Err(tokens),
        }
    }
}

impl TryFrom<Token> for FilterToken {
    type Error = Token;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Filter(filter) => Ok(FilterToken::Group(filter)),
            Token::FilterToken(filter_token) => Ok(filter_token),
            Token::FilterOperation(operation) => Ok(FilterToken::Operation(operation)),
            Token::FilterOperationToken(FilterOperationToken::Postfix(postfix)) => {
                Ok(FilterToken::Postfix(postfix))
            }
            _ => Err(value),
        }
    }
}

impl From<LensParam> for LensIdent {
    fn from(param: LensParam) -> Self {
        param.0
    }
}

impl Token {
    pub fn try_into_query(self) -> Result<Query, Self> {
        if let Self::Query(v) = self { Ok(v) } else { Err(self) }
    }

    pub fn try_into_main(self) -> Result<Query, Self> {
        if let Self::Main(v) = self { Ok(v) } else { Err(self) }
    }
}

impl Range {
    #[must_use]
    pub fn start(&self) -> u32 {
        match self {
            Range::Inclusive { start, .. } | Range::Exclusive { start, .. } => *start,
        }
    }

    #[must_use]
    pub fn end(&self) -> u32 {
        match self {
            Range::Inclusive { end, .. } | Range::Exclusive { end, .. } => *end,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Definition(def) => Display::fmt(def, f),
            Token::Query(query) => Display::fmt(query, f),
            Token::QueryToken(query_token) => Display::fmt(query_token, f),
            Token::Main(query) => Display::fmt(query, f),
            Token::FilterToken(filter_token) => Display::fmt(filter_token, f),
            Token::Filter(filter) => Display::fmt(filter, f),
            Token::FilterOperation(filter_op) => Display::fmt(filter_op, f),
            Token::FilterOperationToken(filter_op_token) => Display::fmt(filter_op_token, f),
            Token::FilterImplicitToken(implicit_token) => Display::fmt(implicit_token, f),
            Token::ExtendSuper(tokens) => {
                for (i, token) in tokens.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", token)?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter().peekable();
        while let Some(token) = iter.next() {
            let Some(next) = iter.peek() else {
                return write!(f, "{}", token);
            };

            match next {
                QueryToken::Infix(_) | QueryToken::Postfix(_) | QueryToken::Filter(_) => {
                    write!(f, "{}", token)?;
                }
                _ => {
                    write!(f, "{}.", token)?;
                }
            }
        }

        Ok(())
    }
}

impl Display for QueryToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryToken::Atom(atom) => Display::fmt(atom, f),
            QueryToken::Molecule(molecule) => Display::fmt(molecule, f),
            QueryToken::Group(molecule) => {
                write!(f, "(")?;
                Display::fmt(molecule, f)?;
                write!(f, ")")
            }
            QueryToken::Infix(op) => Display::fmt(op, f),
            QueryToken::Postfix(postfix) => Display::fmt(postfix, f),
            QueryToken::Filter(filter) => Display::fmt(filter, f),
        }
    }
}

impl Display for Molecule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter().peekable();
        while let Some(token) = iter.next() {
            match token {
                QueryToken::Infix(op) => {
                    write!(f, "{}", op)?;
                    if let Some(next) = iter.peek() {
                        if !matches!(next, QueryToken::Infix(_)) {
                            write!(f, " ")?;
                        }
                    }
                }
                QueryToken::Postfix(postfix) => {
                    write!(f, "{}", postfix)?;
                    if let Some(next) = iter.peek() {
                        if !matches!(next, QueryToken::Postfix(_)) {
                            write!(f, ".")?;
                        }
                    }
                }
                _ => {
                    write!(f, "{}", token)?;

                    if let Some(QueryToken::Postfix(_)) = iter.peek() {
                        // If the next token is a postfix, we don't add a dot
                        continue;
                    }

                    if iter.peek().is_some() {
                        write!(f, ".")?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Index(index) => write!(f, "{}", index),
            Atom::Ident(ident) => write!(f, "{}", ident),
            Atom::String(string) => write!(f, "\"{}\"", string),
            Atom::Lens(ident) => Display::fmt(ident, f),
            Atom::Call(call) => Display::fmt(call, f),
            Atom::Range(range) => Display::fmt(range, f),
            Atom::Wild(wild) => Display::fmt(wild, f),
            Atom::Float(float) => Display::fmt(float, f),
        }
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Float::F32(value) => write!(f, "'{}_f32", value),
            Float::F64(value) => write!(f, "'{}_f64", value),
        }
    }
}

impl Display for LensIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

impl Display for LensCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.lens,
            self.args.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
        )
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Infix::Union => write!(f, " | "),
            Infix::Intersection => write!(f, " & "),
            Infix::Lens(name) => write!(f, "`{}`", name),
        }
    }
}

impl Display for Postfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Postfix::Optional => write!(f, "?"),
            Postfix::Assertion => write!(f, "!"),
            Postfix::Lens(name) => write!(f, "`{}`", name),
        }
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Range::Inclusive { start, end } => write!(f, "{}..={}", start, end),
            Range::Exclusive { start, end } => write!(f, "{}..{}", start, end),
        }
    }
}

impl Display for Wild {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Wild::Once => write!(f, "*"),
            Wild::Exact(count) => write!(f, "*{}", count),
            Wild::Range(range) => write!(f, "*{}", range),
            Wild::Until(tokens) => write!(f, "(**.{})", tokens),
        }
    }
}

impl Display for Definition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.lens)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, "): {};", self.body)
    }
}

impl Display for Filter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "~{{ ")?;
        for (i, token) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", token)?;
        }
        write!(f, " }}")
    }
}

impl Display for FilterToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterToken::Group(filter) => Display::fmt(filter, f),
            FilterToken::Molecule(molecule) => Display::fmt(molecule, f),
            FilterToken::Operation(op) => Display::fmt(op, f),
            FilterToken::Postfix(postfix) => Display::fmt(postfix, f),
        }
    }
}

impl Display for FilterOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl Display for FilterOperationToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterOperationToken::Prefix(prefix) => Display::fmt(prefix, f),
            FilterOperationToken::Infix(infix) => Display::fmt(infix, f),
            FilterOperationToken::Postfix(postfix) => Display::fmt(postfix, f),
            FilterOperationToken::Implicit(implicit) => Display::fmt(implicit, f),
            FilterOperationToken::Atom(atom) => Display::fmt(atom, f),
            FilterOperationToken::Group(filter) => Display::fmt(filter, f),
            FilterOperationToken::Operation(op) => Display::fmt(op, f),
        }
    }
}
impl Display for ImplicitFilterToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImplicitFilterToken::Group(filter) => Display::fmt(filter, f),
            ImplicitFilterToken::Lens(ident) => Display::fmt(ident, f),
            ImplicitFilterToken::Call(call) => Display::fmt(call, f),
            ImplicitFilterToken::Query(query) => Display::fmt(query, f),
        }
    }
}

impl Display for FilterPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterPrefix::Not => write!(f, "!"),
            FilterPrefix::Neg => write!(f, "-"),
        }
    }
}

impl Display for FilterInfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterInfix::Union => write!(f, " | "),
            FilterInfix::Eq => write!(f, "=="),
            FilterInfix::Or => write!(f, "||"),
            FilterInfix::And => write!(f, "&&"),
            FilterInfix::Neq => write!(f, "!="),
            FilterInfix::Lte => write!(f, "<="),
            FilterInfix::Lt => write!(f, "<"),
            FilterInfix::Gte => write!(f, ">="),
            FilterInfix::Gt => write!(f, ">"),
            FilterInfix::Add => write!(f, "+"),
            FilterInfix::Sub => write!(f, "-"),
            FilterInfix::Mul => write!(f, "*"),
            FilterInfix::Div => write!(f, "/"),
            FilterInfix::Lens(ident) => write!(f, "`{}`", ident.0),
        }
    }
}

impl Display for FilterPostfix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilterPostfix::Assertion => write!(f, "!"),
            FilterPostfix::Filter(filter) => Display::fmt(filter, f),
            FilterPostfix::InfixLens(ident) => Display::fmt(ident, f),
        }
    }
}

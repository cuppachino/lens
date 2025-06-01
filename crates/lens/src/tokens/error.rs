use derive_more::{ Display, From };

use super::tokenizer::Rule;

pub type ParsingError = pest::error::Error<Rule>;

#[derive(Debug, Display, From, PartialEq, Eq, Hash)]
pub enum TokenError {
    Parsing(ParsingError),
}

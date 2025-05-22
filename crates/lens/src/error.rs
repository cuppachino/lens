use std::fmt;
use pest::Span;

pub type ParserError = pest::error::Error<crate::ast::parser::Rule>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum LensError<'a> {
    /// `pest` parser error.
    PestError(ParserError),
    /// The input string is empty.
    EmptyInput,
    /// Expected an integer at the beginning of a range.
    ExpectedRangeStart(Span<'a>),
    /// Expected an integer at the end of a range.
    ExpectedRangeEnd(Span<'a>),
    /// Expected a number but found a different token.
    ExpectedNumber(Span<'a>),
}

impl std::error::Error for LensError<'_> {}

impl From<ParserError> for LensError<'_> {
    fn from(err: ParserError) -> Self {
        LensError::PestError(err)
    }
}

impl LensError<'_> {
    /// Create a new `LensError` from a `pest::error::Error`.
    pub fn custom_from_span(message: impl Into<String>, span: Span<'_>) -> Self {
        LensError::PestError(
            ParserError::new_from_span(
                pest::error::ErrorVariant::CustomError { message: message.into() },
                span
            )
        )
    }
}

macro_rules! pretty_print {
    ($f:ident, $span:ident, $expr:expr) => {
        {
            let (line, col) = $span.start_pos().line_col();
            let line_str = $span.start_pos().line_of();
            let caret_pos = " ".repeat(col.saturating_sub(1)) + "^";
            
            {
                $expr
            };

            writeln!($f, " --> lens:{}:{}", line, col)?;
            writeln!($f, "  |\n{:2} | {}", line, line_str)?;
            writeln!($f, "  | {}", caret_pos)
        }
    };
}

impl fmt::Display for LensError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LensError::*;
        match self {
            PestError(err) => err.fmt(f),
            EmptyInput => { writeln!(f, "Error: input string is empty") }
            ExpectedNumber(span) => {
                pretty_print!(f, span, {
                    writeln!(f, "Error: expected a number")?;
                })
            }
            ExpectedRangeStart(span) => {
                pretty_print!(f, span, {
                    writeln!(f, "Error: expected an integer range start")?;
                })
            }
            ExpectedRangeEnd(span) => {
                pretty_print!(f, span, {
                    writeln!(f, "Error: expected an integer range start")?;
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ ast::{ self }, tests::{ parse_lenses, TestResult } };

    #[test]
    fn test_error() -> TestResult<'static> {
        let input = "x**4";
        let LensError::PestError(err) = parse_lenses(input).unwrap_err() else {
            panic!("Expected a PestError");
        };
        match err.variant {
            pest::error::ErrorVariant::ParsingError { positives, negatives } => {
                assert_eq!(positives, vec![ast::parser::Rule::EOI]);
                assert_eq!(negatives, vec![]);
            }
            _ => {
                panic!("Expected a ParsingError");
            }
        }
        Ok(())
    }
}

use pest::iterators::{ Pair, Pairs };

use pest_derive::Parser;

use crate::error::LensError;

/// An iterator over lens expressions parsed from a string.
///
/// This iterator processes multiple lens expressions separated by semicolons.
///
/// The final element in the iterator is always `Expr::EOI`, which indicates the end of input.
pub struct LensesIter<'a, I> where I: Iterator<Item = Result<Expr<'a>, LensError<'a>>> {
    inner: I,
}

impl<'a, I: Iterator<Item = Result<Expr<'a>, LensError<'a>>>> LensesIter<'a, I> {
    /// Create a new `LensIter` from an iterator.
    #[inline]
    pub const fn new(inner: I) -> Self {
        LensesIter { inner }
    }
}

impl<'a, I> Iterator for LensesIter<'a, I> where I: Iterator<Item = Result<Expr<'a>, LensError<'a>>> {
    type Item = Result<Expr<'a>, LensError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// Type alias for the result of mapping pest pairs to lens expressions.
pub type MapBuildSegmentIter<'a> = std::iter::Map<
    Pairs<'a, parser::Rule>,
    fn(Pair<'_, parser::Rule>) -> Result<Expr<'_>, LensError<'_>>
>;

impl<'a> TryFrom<&'a str> for LensesIter<'a, MapBuildSegmentIter<'a>> {
    type Error = LensError<'a>;

    /// Attempts to create a `LensesIter` from a string.
    ///
    /// # Errors
    ///
    /// Returns an error if the input string cannot be parsed.
    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
        use pest::Parser;
        let lens_list_list = parser::LensParser::parse(parser::Rule::lenses, input)?;
        let lens_list = lens_list_list.into_iter().next().ok_or(LensError::EmptyInput)?;
        let inner: MapBuildSegmentIter<'_> = lens_list.into_inner().map(parser::build_segment);

        Ok(LensesIter { inner })
    }
}

#[cfg(test)]
mod test_iter {
    use super::*;
    use crate::tests::TestResult;

    #[test]
    fn test_lenses_iter() -> TestResult<'static> {
        let input = "foo.bar;baz.qux;";
        let expected = &[
            Expr::Lens(vec![Expr::Identifier("foo"), Expr::Identifier("bar")]),
            Expr::Lens(vec![Expr::Identifier("baz"), Expr::Identifier("qux")]),
            Expr::EOI,
        ];
        let iter = LensesIter::try_from(input)?;
        let exprs: Vec<_> = iter.collect::<Result<Vec<_>, _>>()?;

        println!("exprs: {:?}", exprs);
        assert_eq!(exprs, expected);

        Ok(())
    }
}

/// Represents an expression in the lens query language.
///
/// The lens query language allows for traversing and filtering structured data
/// using a path-like syntax. Expressions can be simple identifiers, indexes,
/// wildcards, ranges, or combinations of these.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Expr<'a> {
    /// A complete lens expression path.
    ///
    /// This represents a sequence of segments forming a path, such as `foo.bar.baz`.
    Lens(Vec<Expr<'a>>),

    /// A simple identifier segment.
    ///
    /// # Examples
    ///
    /// `foo` in `foo.bar`
    Identifier(&'a str),

    /// A quoted identifier segment, which can contain special characters.
    ///
    /// # Examples
    ///
    /// `"foo.bar"` in `"foo.bar".baz`
    QuotedIdentifier(&'a str),

    /// A numeric index segment.
    ///
    /// # Examples
    ///
    /// `3` in `array.3`
    Index(u32),

    /// A range segment, which can be inclusive or exclusive.
    ///
    /// # Examples
    ///
    /// `1..3` or `1..=3` in `array.1..3`
    Range(Range),

    /// A wildcard segment matching any single element.
    ///
    /// # Examples
    ///
    /// `*` in `array.*`
    Wildcard,

    /// A wildcard segment matching exactly N elements.
    ///
    /// # Examples
    ///
    /// `*3` in `array.*3`
    WildcardN(u32),

    /// A wildcard range segment matching a range of elements.
    ///
    /// # Examples
    ///
    /// `*..3`, `*..=3`, or `*2..=3`
    WildcardRange(Range),

    /// A recursive wildcard segment matching until a specific pattern is found.
    ///
    /// # Examples
    ///
    /// `**.token` in `data.**.token`
    WildcardUntil(Vec<Expr<'a>>),

    /// An optional segment that may or may not exist.
    ///
    /// # Examples
    ///
    /// `foo?` in `root.foo?.bar`
    Optional(Box<Expr<'a>>),

    /// A union of alternative segments.
    ///
    /// # Examples
    ///
    /// `a|b|"c**c"` in `data.(a|b|"c**c")`
    Union(Vec<Expr<'a>>),

    /// End of input marker.
    EOI,
}

impl Expr<'_> {
    /// Creates a new quoted identifier from a string.
    ///
    /// This function strips leading and trailing quotes from the string.
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Expr;
    ///
    /// let quoted = Expr::quoted("\"foo.bar\"");
    /// if let Expr::QuotedIdentifier(ident) = quoted {
    ///     assert_eq!(ident, "foo.bar");
    /// }
    /// ```
    pub fn quoted(ident: &str) -> Expr<'_> {
        let ident = ident.trim_matches(&['\'', '"']);
        Expr::QuotedIdentifier(ident)
    }

    /// Converts the expression into an optional expression.
    ///
    /// This wraps the expression in `Expr::Optional`. It does
    /// not check if the expression is already optional.
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Expr;
    ///
    /// let ident = Expr::Identifier("foo");
    /// let optional = ident.into_optional();
    ///
    /// if let Expr::Optional(boxed) = optional {
    ///     if let Expr::Identifier(name) = *boxed {
    ///         assert_eq!(name, "foo");
    ///     }
    /// }
    /// ```
    #[inline]
    pub fn into_optional(self) -> Self {
        Expr::Optional(self.into())
    }

    /// Returns `true` if the expression is a `Lens`.
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Expr;
    ///
    /// let path = Expr::Lens(vec![Expr::Identifier("foo"), Expr::Identifier("bar")]);
    /// assert!(path.is_lens());
    ///
    /// let ident = Expr::Identifier("foo");
    /// assert!(!ident.is_lens());
    /// ```
    #[must_use]
    pub fn is_lens(&self) -> bool {
        matches!(self, Self::Lens(..))
    }
}

/// Represents a range in a structured query.
///
/// Ranges can be inclusive (`start..=end`) or exclusive (`start..end`).
/// Both types support implicit start values (e.g., `..end` or `..=end`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Range {
    /// An inclusive range (`..=end` or `start..=end`).
    ///
    /// Represents values from `start` up to and including `end`.
    Inclusive {
        /// The start of the range (inclusive).
        start: u32,
        /// The end of the range (inclusive).
        end: u32,
    },
    /// An exclusive range (`..end` or `start..end`).
    ///
    /// Represents values from `start` up to but not including `end`.
    Exclusive {
        /// The start of the range (inclusive).
        start: u32,
        /// The end of the range (exclusive).
        end: u32,
    },
}

impl Range {
    /// Creates a new inclusive range (`start..=end`).
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Range;
    ///
    /// let range = Range::inclusive(1, 5);
    /// assert_eq!(range.start(), 1);
    /// assert_eq!(range.end(), 5);
    /// ```
    #[inline]
    pub const fn inclusive(start: u32, end: u32) -> Self {
        Range::Inclusive { start, end }
    }

    /// Creates a new exclusive range (`start..end`).
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Range;
    ///
    /// let range = Range::exclusive(1, 5);
    /// assert_eq!(range.start(), 1);
    /// assert_eq!(range.end(), 5);
    /// ```
    #[inline]
    pub const fn exclusive(start: u32, end: u32) -> Self {
        Range::Exclusive { start, end }
    }

    /// Returns the start of the range.
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Range;
    ///
    /// let inclusive = Range::inclusive(1, 5);
    /// assert_eq!(inclusive.start(), 1);
    ///
    /// let exclusive = Range::exclusive(2, 6);
    /// assert_eq!(exclusive.start(), 2);
    /// ```
    #[inline]
    #[must_use]
    pub fn start(&self) -> u32 {
        match self {
            Range::Inclusive { start, .. } => *start,
            Range::Exclusive { start, .. } => *start,
        }
    }

    /// Returns the end of the range.
    ///
    /// # Examples
    ///
    /// ```
    /// use lens::ast::Range;
    ///
    /// let inclusive = Range::inclusive(1, 5);
    /// assert_eq!(inclusive.end(), 5);
    ///
    /// let exclusive = Range::exclusive(2, 6);
    /// assert_eq!(exclusive.end(), 6);
    /// ```
    #[inline]
    #[must_use]
    pub fn end(&self) -> u32 {
        match self {
            Range::Inclusive { end, .. } => *end,
            Range::Exclusive { end, .. } => *end,
        }
    }
}

pub mod parser {
    use super::*;

    /// Pest parser for the lens query language.
    #[derive(Parser)]
    #[grammar = "lens.pest"]
    pub struct LensParser;

    /// Parses a lens expression from a pest pair.
    pub fn build_segment(pair: Pair<Rule>) -> Result<Expr, LensError> {
        match pair.as_rule() {
            Rule::identifier => Ok(Expr::Identifier(pair.as_str())),

            Rule::quoted => {
                let s = pair.as_str();
                Ok(Expr::quoted(s))
            }

            Rule::index => {
                let v = pair.as_str().parse().unwrap();
                Ok(Expr::Index(v))
            }

            Rule::range => {
                let is_implicit = pair.as_str().starts_with("..");
                let (start, end) = parse_range(pair, is_implicit)?;

                Ok(Expr::Range(Range::exclusive(start, end)))
            }

            Rule::range_inclusive => {
                let is_implicit = pair.as_str().starts_with("..=");
                let (start, end) = parse_range(pair, is_implicit)?;

                Ok(Expr::Range(Range::inclusive(start, end)))
            }

            Rule::wildcard => Ok(Expr::Wildcard),

            Rule::wildcard_exact => {
                let n = pair.into_inner().next().unwrap().as_str();
                let n = n.parse::<u32>().unwrap();

                Ok(Expr::WildcardN(n))
            }

            Rule::wildcard_range => {
                let is_implicit = pair.as_str().starts_with("*..");
                let (start, end) = parse_range(pair, is_implicit)?;

                Ok(Expr::WildcardRange(Range::exclusive(start, end)))
            }

            Rule::wildcard_range_inclusive => {
                let is_implicit = pair.as_str().starts_with("*..=");
                let (start, end) = parse_range(pair, is_implicit)?;

                Ok(Expr::WildcardRange(Range::inclusive(start, end)))
            }

            Rule::wildcard_recursive => {
                let span_str = pair.as_str();
                let mut components = Vec::new();
                for pair in pair.into_inner() {
                    let idx = pair.as_span().end();
                    let is_optional = span_str
                        .get(idx..idx + 1)
                        .map(|c| c == "?")
                        .unwrap_or_default();
                    if is_optional {
                        components.push(build_segment(pair)?.into_optional());
                    } else {
                        components.push(build_segment(pair)?);
                    }
                }

                Ok(Expr::WildcardUntil(components))
            }

            Rule::expr => {
                let is_optional = pair.as_str().ends_with('?');
                let span = pair.as_span();
                let mut inner = pair.into_inner();
                let token = build_segment(
                    inner.next().ok_or(
                        pest::error::Error::new_from_span(
                            pest::error::ErrorVariant::CustomError {
                                message: "Expected expression".into(),
                            },
                            span
                        )
                    )?
                )?;

                Ok(if is_optional { token.into_optional() } else { token })
            }

            Rule::union => {
                let variants = pair.into_inner().map(build_segment).collect::<Result<Vec<_>, _>>()?;

                Ok(Expr::Union(variants))
            }

            Rule::lens => {
                let inner = pair.into_inner();
                let segments = inner.map(build_segment).collect::<Result<Vec<_>, _>>()?;

                Ok(Expr::Lens(segments))
            }

            Rule::EOI => Ok(Expr::EOI),

            _ => panic!("Unexpected rule: {:?}", pair.as_rule()),
        }
    }

    pub fn parse_range<'a>(
        pair: Pair<'a, Rule>,
        is_implicit: bool
    ) -> Result<(u32, u32), LensError<'a>> {
        let span = pair.as_span();
        let mut inner = pair.into_inner();
        let start = if is_implicit {
            0
        } else {
            inner
                .next()
                .ok_or_else(|| LensError::ExpectedRangeStart(span))?
                .as_str()
                .parse::<u32>()
                .map_err(|_| LensError::ExpectedNumber(span))?
        };
        let end = inner
            .next()
            .ok_or_else(|| LensError::ExpectedRangeEnd(span))?
            .as_str()
            .parse::<u32>()
            .map_err(|_| LensError::ExpectedNumber(span))?;

        Ok((start, end))
    }
}

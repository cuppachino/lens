mod ast;
mod error;

/// `use lens::prelude::*;` to import common traits, types, and functions.
pub mod prelude {
    pub use crate::ast::{ Expr, LensesIter, Range };
    pub use crate::error::{ LensError, ParserError };
}

#[cfg(test)]
mod tests {
    use crate::{ ast::{ Expr, LensesIter, Range }, error::LensError };

    use super::*;

    pub type TestResult<'a> = Result<(), LensError<'a>>;

    #[inline]
    pub(crate) fn pretty<D: std::fmt::Display>(err: D) -> D {
        eprintln!("{err}");
        err
    }

    #[inline]
    pub(crate) fn single_lens(input: &str) -> Result<Vec<Expr<'_>>, LensError> {
        let lenses = LensesIter::try_from(input).map_err(pretty)?.collect::<Result<Vec<_>, _>>()?;

        (
            match lenses.into_iter().next() {
                Some(Expr::Lens(exprs)) => { Ok(exprs) }
                None => { Err(LensError::EmptyInput) }
                _ => {
                    Err(
                        LensError::PestError(
                            pest::error::Error::new_from_span(
                                pest::error::ErrorVariant::CustomError {
                                    message: "Expected a single lens".into(),
                                },
                                pest::Span::new(input, 0, input.len()).unwrap()
                            )
                        )
                    )
                }
            }
        ).map_err(pretty)
    }

    #[inline]
    pub(crate) fn single_token(input: &str) -> Result<Expr<'_>, LensError> {
        single_lens(input)?.into_iter().next().ok_or(LensError::EmptyInput)
    }

    macro_rules! first_token {
        ($input:expr, $expected:expr) => {
            let input = $input;
            let expected = $expected;
            let token = single_token(input)?;
            println!("{input}: {:?}", token);
            assert_eq!(token, expected);
        };
    }

    pub fn parse_lenses(input: &str) -> Result<Vec<ast::Expr<'_>>, error::LensError> {
        LensesIter::try_from(input)?.collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn multi_lens() -> TestResult<'static> {
        let input = "a.*";
        let expected = &[Expr::Lens(vec![Expr::Identifier("a"), Expr::Wildcard])];
        let mut paths = parse_lenses(input)?;
        let last = paths.pop();
        assert_eq!(last, Some(Expr::EOI));

        println!("{input}: {:?}", paths);
        assert_eq!(paths.len(), 1);
        assert_eq!(paths, expected);

        let input = "a.*;b.**";
        let expected = &[
            Expr::Lens(vec![Expr::Identifier("a"), Expr::Wildcard]),
            Expr::Lens(vec![Expr::Identifier("b"), Expr::WildcardUntil(vec![])]),
        ];
        let mut paths = parse_lenses(input)?;
        let last = paths.pop();
        assert_eq!(last, Some(Expr::EOI));
        println!("{input}: {:?}", paths);
        assert_eq!(paths.len(), 2);
        assert_eq!(paths, expected);

        let input = "a.*;b.**;c.*";
        let expected = &[
            Expr::Lens(vec![Expr::Identifier("a"), Expr::Wildcard]),
            Expr::Lens(vec![Expr::Identifier("b"), Expr::WildcardUntil(vec![])]),
            Expr::Lens(vec![Expr::Identifier("c"), Expr::Wildcard]),
        ];
        let mut paths = parse_lenses(input)?;
        let last = paths.pop();
        assert_eq!(last, Some(Expr::EOI));
        println!("{input}: {:?}", paths);
        assert_eq!(paths.len(), 3);
        assert_eq!(paths, expected);
        Ok(())
    }

    #[test]
    fn quoted() -> TestResult<'static> {
        let input = "'a'";
        let expected = Expr::QuotedIdentifier("a");
        first_token!(input, expected);
        Ok(())
    }

    #[test]
    fn optional() -> TestResult<'static> {
        let input = "a?";
        let expected = Expr::Identifier("a").into_optional();
        first_token!(input, expected);

        let input = "a.b.c?";
        let expected = &[
            Expr::Identifier("a"),
            Expr::Identifier("b"),
            Expr::Identifier("c").into_optional(),
        ];
        let tokens = single_lens(input)?;
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens, expected);
        Ok(())
    }

    #[test]
    fn union() -> TestResult<'static> {
        let input = "a|\"b\"|'c'";
        let expected = Expr::Union(
            vec![Expr::Identifier("a"), Expr::QuotedIdentifier("b"), Expr::QuotedIdentifier("c")]
        );
        first_token!(input, expected);
        Ok(())
    }

    #[test]
    fn index() -> TestResult<'static> {
        let input = "0";
        let expected = Expr::Index(0);
        first_token!(input, expected);

        let input = "a.0";
        let expected = &[Expr::Identifier("a"), Expr::Index(0)];
        let tokens = single_lens(input)?;
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens, expected);
        Ok(())
    }

    #[test]
    fn range() -> TestResult<'static> {
        let input = "0..2";
        let expected = Expr::Range(Range::exclusive(0, 2));
        first_token!(input, expected);

        let input = "0..=2";
        let expected = Expr::Range(Range::inclusive(0, 2));
        first_token!(input, expected);

        let input = "a.0.0..=2";
        let expected = &[
            Expr::Identifier("a"),
            Expr::Index(0),
            Expr::Range(Range::inclusive(0, 2)),
        ];
        let tokens = single_lens(input)?;
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens, expected);
        Ok(())
    }

    #[test]
    fn wildcard() -> TestResult<'static> {
        let input = "*";
        let expected = Expr::Wildcard;
        first_token!(input, expected);
        Ok(())
    }

    #[test]
    fn wildcard_exact() -> TestResult<'static> {
        let input = "*3";
        let expected = Expr::WildcardN(3);
        first_token!(input, expected);
        Ok(())
    }

    #[test]
    fn wildcard_until_none() -> TestResult<'static> {
        let input = "**";
        let expected = Expr::WildcardUntil(vec![]);
        first_token!(input, expected);
        Ok(())
    }

    #[test]
    fn wildcard_until_some_some() -> TestResult<'static> {
        let input = "**.token.name";
        let expected = &[
            Expr::WildcardUntil(vec![Expr::Identifier("token")]),
            Expr::Identifier("name"),
        ];
        let tokens = single_lens(input)?;
        println!("{input}: {:?}", tokens);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn wildcard_until_optional_some() -> TestResult<'static> {
        let input = "**.token?.name.*";
        let expected = &[
            Expr::WildcardUntil(
                vec![Expr::Identifier("token").into_optional(), Expr::Identifier("name")]
            ),
            Expr::Wildcard,
        ];
        let tokens = single_lens(input)?;
        println!("{input}: {:?}", tokens);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn wildcard_range_inclusive() -> TestResult<'static> {
        let input = "*..=2";
        let expected = Expr::WildcardRange(Range::inclusive(0, 2));
        first_token!(input, expected);

        let input = "*1..=2";
        let expected = Expr::WildcardRange(Range::inclusive(1, 2));
        first_token!(input, expected);

        let input = "a.*..=2";
        let expected = &[Expr::Identifier("a"), Expr::WildcardRange(Range::inclusive(0, 2))];
        let tokens = single_lens(input)?;
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn wildcard_range_exclusive() -> TestResult<'static> {
        let input = "*..2";
        let expected = Expr::WildcardRange(Range::exclusive(0, 2));
        first_token!(input, expected);

        let input = "*1..2";
        let expected = Expr::WildcardRange(Range::exclusive(1, 2));
        first_token!(input, expected);

        let input = "a.*..2";
        let expected = &[Expr::Identifier("a"), Expr::WildcardRange(Range::exclusive(0, 2))];
        let tokens = single_lens(input)?;
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn grouping() -> TestResult<'static> {
        let input = "a|(b.*.0..4).*3..=4.info?.name";
        let expected = &[
            Expr::Union(
                vec![
                    Expr::Identifier("a"),
                    Expr::Lens(
                        vec![
                            Expr::Identifier("b"),
                            Expr::Wildcard,
                            Expr::Range(Range::exclusive(0, 4))
                        ]
                    )
                ]
            ),
            Expr::WildcardRange(Range::inclusive(3, 4)),
            Expr::Identifier("info").into_optional(),
            Expr::Identifier("name"),
        ];
        let tokens = single_lens(input)?;
        println!("{input}: {:?}", tokens);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn everything() -> TestResult<'static> {
        let input = "a|b.*.0..=2.*3..=4.info?.name;(c.**.age)|(d.*.number)";
        let expected = &[
            Expr::Union(vec![Expr::Identifier("a"), Expr::Identifier("b")]),
            Expr::Wildcard,
            Expr::Range(Range::inclusive(0, 2)),
            Expr::WildcardRange(Range::inclusive(3, 4)),
            Expr::Identifier("info").into_optional(),
            Expr::Identifier("name"),
        ];
        let tokens = single_lens(input)?;
        println!("{input}: {:?}", tokens);
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens, expected);

        Ok(())
    }

    #[test]
    fn readme_example() -> Result<(), Box<dyn std::error::Error>> {
        use crate::prelude::*;

        let mut iter = LensesIter::try_from("departments.**.staff.*")?;
        for expr in iter.by_ref().take(1) {
            let expr = expr?;
            match expr {
                Expr::Lens(exprs) => {
                    assert_eq!(exprs.len(), 3);
                    assert_eq!(exprs[0], Expr::Identifier("departments"));
                    assert_eq!(exprs[1], Expr::WildcardUntil(vec![Expr::Identifier("staff")]));
                    assert_eq!(exprs[2], Expr::Wildcard);
                }
                _ => unreachable!(),
            }
        }

        assert!(matches!(iter.next(), Some(Ok(Expr::EOI))));
        Ok(())
    }
}

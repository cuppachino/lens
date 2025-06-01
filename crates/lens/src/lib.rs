pub mod tokens;

#[cfg(test)]
mod tests {
    use crate::tokens::{ error::TokenError, prelude::*, tokenizer::Tokenizer };

    type TestResult<T = ()> = Result<T, TokenError>;

    fn main_query(input: &str) -> Result<Token, TokenError> {
        let tokens = Tokenizer::from_input(input)
            .map_err(pretty)?
            .collect::<Result<Vec<Token>, _>>()?;
        let token = tokens.into_iter().last().unwrap();
        Ok(token)
    }

    fn first_main_filter(input: &str) -> Result<Filter, TokenError> {
        let tokens = Tokenizer::from_input(input)
            .map_err(pretty)?
            .collect::<Result<Vec<Token>, _>>()?;

        let filter: Filter = tokens
            .into_iter()
            .find_map(|t| {
                if let Token::Main(query) = t {
                    query.into_iter().find_map(|qt| {
                        match qt {
                            QueryToken::Filter(filter) => { Some(filter) }
                            QueryToken::Molecule(mol) =>
                                mol.into_iter().find_map(|qt| {
                                    if let QueryToken::Filter(filter) = qt {
                                        Some(filter)
                                    } else {
                                        None
                                    }
                                }),
                            _ => { None }
                        }
                    })
                } else {
                    None
                }
            })
            .expect("No filter found in main query");

        Ok(filter)
    }

    macro_rules! assert_main_query_eq {
        ($input:expr, $expected:expr, debug) => {
            let actual = main_query($input)?;
            println!("Resolved Type: {:#?}", actual);
            println!("Input: {}", $input);
            println!("Resolved: {actual}");
            assert_eq!(actual, $expected);
        };
        ($input:expr, $expected:expr) => {
            let actual = main_query($input)?;
            assert_eq!(actual, $expected);
        };
    }

    macro_rules! assert_first_main_filter_eq {
        ($input:expr, $expected:expr) => {
            let actual = first_main_filter($input)?;
            assert_eq!(actual, $expected);
        };
    }

    #[inline]
    pub(crate) fn pretty<D: std::fmt::Display>(err: D) -> D {
        eprintln!("{err}");
        err
    }

    /* #[test]
    fn goal() -> TestResult {
        // todo: lambda lens
        let input =
            r#"
            'names: **.departments.**.staff.name;
            'is_name('name): ~ { name == 'name };
            'age('name): **.users.**.*~{ 'is_name('name) }.age;
            'user('name): -> { name = 'name, age = 'age('name) };
            'user('names)
        "#.trim();

        let tokens = Tokenizer::from_input(input)
            .map_err(pretty)?
            .collect::<Result<Vec<Token>, _>>()?;
        Ok(())
    } */

    #[test]
    fn string() -> TestResult {
        let input = "\"a\"";
        let expected = Token::Main(vec![QueryToken::Atom(Atom::String(input.to_string()))].into());
        assert_main_query_eq!(input, expected);

        let input = r#""a.b.c""#;
        let expected = Token::Main(vec![QueryToken::Atom(Atom::String(input.to_string()))].into());
        assert_main_query_eq!(input, expected);
        Ok(())
    }

    #[test]
    fn index() -> TestResult {
        let input = "0";
        let expected = Token::Main(vec![QueryToken::Atom(Atom::Index(0))].into());
        assert_main_query_eq!(input, expected);

        let input = "a.0";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    vec![
                        QueryToken::Atom(Atom::Ident("a".to_string())),
                        QueryToken::Atom(Atom::Index(0))
                    ].into()
                )
            ].into()
        );
        assert_main_query_eq!(input, expected);
        Ok(())
    }

    #[test]
    fn float_basic() -> TestResult {
        let expected = Filter(
            vec![
                FilterToken::Operation(
                    FilterOperation(vec![FilterOperationToken::Atom(Atom::Float(Float::F64(3.14)))])
                )
            ]
        );
        let input = "*~{ '3.14_f64 }";
        assert_first_main_filter_eq!(input, expected);
        let input = "*~{ '3.14f64 }";
        assert_first_main_filter_eq!(input, expected);

        let expected = Filter(
            vec![
                FilterToken::Operation(
                    FilterOperation(vec![FilterOperationToken::Atom(Atom::Float(Float::F32(3.14)))])
                )
            ]
        );
        let input = "*~{ '3.14_f32 }";
        assert_first_main_filter_eq!(input, expected);
        let input = "*~{ '3.14f32 }";
        assert_first_main_filter_eq!(input, expected);

        let expected = Filter(
            vec![
                FilterToken::Operation(
                    FilterOperation(vec![FilterOperationToken::Atom(Atom::Float(Float::F32(3.0)))])
                )
            ]
        );
        let input = "*~{ '3.0f32 }";
        assert_first_main_filter_eq!(input, expected);
        let input = "*~{ '3.0_f32 }";
        assert_first_main_filter_eq!(input, expected);
        Ok(())
    }

    #[test]
    fn float_scientific() -> TestResult {
        // in a query, floats shouldn't be parsed at all.
        let input = "'3.14e2_f64";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    Molecule(
                        vec![
                            QueryToken::Atom(
                                Atom::Call(LensCall {
                                    lens: "3".to_string().into(),
                                    args: vec![],
                                })
                            ),
                            QueryToken::Atom(Atom::Ident("14e2_f64".to_string()))
                        ]
                    )
                )
            ].into()
        );
        assert_main_query_eq!(input, expected);

        // but in a filter they should be parsed as floats.
        let input = "*~{ '3.14e2_f64 }";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    vec![
                        QueryToken::Atom(Atom::Wild(Wild::Once)),
                        QueryToken::Filter(
                            Filter(
                                vec![
                                    FilterToken::Operation(
                                        FilterOperation(
                                            vec![
                                                FilterOperationToken::Atom(
                                                    Atom::Float(Float::F64(3.14e2))
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
                    ].into()
                )
            ].into()
        );
        assert_main_query_eq!(input, expected);

        // check other other sign
        let input = "*~{ '3.14e+2f64 }";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    vec![
                        QueryToken::Atom(Atom::Wild(Wild::Once)),
                        QueryToken::Filter(
                            Filter(
                                vec![
                                    FilterToken::Operation(
                                        FilterOperation(
                                            vec![
                                                FilterOperationToken::Atom(
                                                    Atom::Float(Float::F64(3.14e2))
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
                    ].into()
                )
            ].into()
        );
        assert_main_query_eq!(input, expected);

        Ok(())
    }

    #[test]
    fn float_negated() -> TestResult {
        let input = "a~{ -'2_f64 }";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    vec![
                        QueryToken::Atom(Atom::Ident("a".to_string())),
                        QueryToken::Filter(
                            Filter(
                                vec![
                                    FilterToken::Operation(
                                        FilterOperation(
                                            vec![
                                                FilterOperationToken::Prefix(FilterPrefix::Neg),
                                                FilterOperationToken::Atom(
                                                    Atom::Float(Float::F64(2.0))
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        )
                    ].into()
                )
            ].into()
        );

        assert_main_query_eq!(input, expected);

        Ok(())
    }

    #[test]
    fn fail_query_postfix() -> TestResult {
        let input = "a??";
        let err = Tokenizer::from_input(input).expect_err("illegal double postfix");
        assert!(matches!(err, TokenError::Parsing(_)));

        let input = "a?!";
        let err = Tokenizer::from_input(input).expect_err("illegal double postfix");
        assert!(matches!(err, TokenError::Parsing(_)));

        let input = "a!a";
        let err = Tokenizer::from_input(input).expect_err("postfix between atoms, not a query");
        assert!(matches!(err, TokenError::Parsing(_)));

        let input = "a~{ b }?!";
        let err = Tokenizer::from_input(input).expect_err("illegal triple postfix combination");
        assert!(matches!(err, TokenError::Parsing(_)));

        let input = "a~{ b }!?";
        let err = Tokenizer::from_input(input).expect_err("illegal triple postfix combination");
        assert!(matches!(err, TokenError::Parsing(_)));
        Ok(())
    }

    #[test]
    fn pass_query_postfix() -> TestResult {
        let input = "a?.b!.c~{ d }!";
        let expected = Token::Main(
            vec![
                QueryToken::Molecule(
                    vec![
                        QueryToken::Atom(Atom::Ident("a".to_string())),
                        QueryToken::Postfix(Postfix::Optional),
                        QueryToken::Atom(Atom::Ident("b".to_string())),
                        QueryToken::Postfix(Postfix::Assertion),
                        QueryToken::Atom(Atom::Ident("c".to_string())),
                        QueryToken::Filter(
                            vec![
                                FilterToken::Operation(
                                    FilterOperation(
                                        vec![
                                            FilterOperationToken::Atom(Atom::Ident("d".to_string()))
                                        ]
                                    )
                                )
                            ].into()
                        ),
                        QueryToken::Postfix(Postfix::Assertion)
                    ].into()
                )
            ].into()
        );

        assert_main_query_eq!(input, expected);

        Ok(())
    }

    #[test]
    fn query_infix() -> TestResult {
        let input = "0 | a | b & c | d & (e & f | g) `and` h `and` `foo` 1";
        let expected = Token::Main(
            vec![
                QueryToken::Atom(Atom::Index(0)),
                QueryToken::Infix(Infix::Union),
                QueryToken::Atom(Atom::Ident("a".to_string())),
                QueryToken::Infix(Infix::Union),

                QueryToken::Atom(Atom::Ident("b".to_string())),
                QueryToken::Infix(Infix::Intersection),
                QueryToken::Atom(Atom::Ident("c".to_string())),

                QueryToken::Infix(Infix::Union),

                QueryToken::Atom(Atom::Ident("d".to_string())),
                QueryToken::Infix(Infix::Intersection),
                QueryToken::Group(
                    vec![
                        QueryToken::Atom(Atom::Ident("e".to_string())),
                        QueryToken::Infix(Infix::Intersection),
                        QueryToken::Atom(Atom::Ident("f".to_string())),
                        QueryToken::Infix(Infix::Union),
                        QueryToken::Atom(Atom::Ident("g".to_string()))
                    ].into()
                ),
                QueryToken::Infix(Infix::Lens("and".to_string().into())),
                QueryToken::Atom(Atom::Ident("h".to_string())),
                QueryToken::Infix(Infix::Lens("and".to_string().into())),
                QueryToken::Infix(Infix::Lens("foo".to_string().into())),
                QueryToken::Atom(Atom::Index(1))
            ].into()
        );

        assert_main_query_eq!(input, expected);
        Ok(())
    }

    #[test]
    fn query_def() -> TestResult {
        let input = "'foo('a, 'b): 'b?.'a(1) | 'b~{ 'c }; 'foo(a, b)";
        let expected = Token::Definition(Definition {
            lens: LensIdent("foo".into()),
            params: vec![LensParam(LensIdent("a".into())), LensParam(LensIdent("b".into()))],
            body: LensBody::Query(
                vec![
                    QueryToken::Molecule(
                        vec![
                            QueryToken::Atom(
                                Atom::Call(LensCall {
                                    lens: LensIdent("b".to_string()),
                                    args: vec![],
                                })
                            ),
                            QueryToken::Postfix(Postfix::Optional),
                            QueryToken::Atom(
                                Atom::Call(LensCall {
                                    lens: LensIdent("a".to_string()),
                                    args: vec![Token::QueryToken(QueryToken::Atom(Atom::Index(1)))],
                                })
                            )
                        ].into()
                    ),
                    QueryToken::Infix(Infix::Union),
                    QueryToken::Molecule(
                        vec![
                            QueryToken::Atom(
                                Atom::Call(LensCall {
                                    lens: LensIdent("b".to_string()),
                                    args: vec![],
                                })
                            ),
                            QueryToken::Filter(
                                Filter(
                                    vec![
                                        FilterToken::Operation(
                                            FilterOperation(
                                                vec![
                                                    FilterOperationToken::Implicit(
                                                        ImplicitFilterToken::Call(LensCall {
                                                            lens: LensIdent("c".into()),
                                                            args: vec![],
                                                        })
                                                    )
                                                ]
                                            )
                                        )
                                    ]
                                )
                            )
                        ].into()
                    )
                ].into()
            ),
        });

        let tok = Tokenizer::from_input(input).map_err(pretty)?;
        let tokens = tok.collect::<Result<Vec<Token>, _>>()?;

        let def = tokens.into_iter().next().unwrap();
        assert_eq!(def, expected);

        Ok(())
    }
}

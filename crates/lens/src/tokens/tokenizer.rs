use std::iter::FusedIterator;

use pest::{ iterators::{ Pair as _Pair, Pairs as _Pairs }, Parser };
use pest_derive::Parser as PestParser;

use super::{ error::TokenError, prelude::*, token::filter::* };

pub type Pair<'a> = _Pair<'a, Rule>;
pub type Pairs<'a> = _Pairs<'a, Rule>;

/// A lazy (but greedy) tokenizer iterator that converts a Pest parse tree into a sequence of tokens.
#[derive(Clone, Debug, PestParser)]
#[grammar = "lens.pest"]
pub struct Tokenizer<'input> {
    stack: Vec<Pair<'input>>,
    pending: Vec<Token>,
    context: Vec<ProcessingContext>,
}

#[derive(Clone, Debug)]
enum ProcessingContext {
    Main {
        collected_tokens: Vec<Token>,
        remaining_pairs: usize,
    },
    Query {
        collected_tokens: Vec<Token>,
        remaining_pairs: usize,
    },
    QueryGroup {
        collected_tokens: Vec<QueryToken>,
        remaining_pairs: usize,
    },
    FilterGroup {
        collected_tokens: Vec<FilterToken>,
        remaining_pairs: usize,
    },
    Molecule {
        collected_tokens: Vec<QueryToken>,
        remaining_pairs: usize,
    },
    QueryDefinition {
        lens: LensIdent,
        params: Vec<LensParam>,
        body: Query,
        remaining_pairs: usize,
        remaining_params: usize,
        skip_first: bool,
    },
    FilterDefinition {
        lens: LensIdent,
        params: Vec<LensParam>,
        body: Filter,
        remaining_pairs: usize,
        remaining_params: usize,
        skip_first: bool,
    },
    LensCall {
        name: String,
        args: Vec<Token>,
        remaining_args: usize,
    },
    RecursiveWildcard {
        until_tokens: Vec<Token>,
        remaining_pairs: usize,
    },
    Filter {
        collected_tokens: Vec<FilterToken>,
        remaining_pairs: usize,
    },
    FilterOperation {
        collected_tokens: Vec<FilterOperationToken>,
        remaining_pairs: usize,
    },
    FilterImplicitOperation {
        collected_tokens: Vec<Token>,
        remaining_pairs: usize,
    },
    FilterPostfix {
        collected_tokens: Vec<FilterOperationToken>,
        remaining_pairs: usize,
    },
    PathPostfix {
        collected_tokens: Vec<Token>,
        remaining_pairs: usize,
    },
}

impl<'input> FusedIterator for Tokenizer<'input> {}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Exit out of the loop if we have pending tokens.
            if let Some(token) = self.pending.pop() {
                return Some(Ok(token));
            }

            // Handle contexts first
            if let Some(ctx) = self.context.pop() {
                match self.try_finalize_context(ctx) {
                    Ok(token) => {
                        // If the context is finalized, handle the token.
                        // It's important we allow any parent context to handle this token.
                        self.handle_token_for_context(token);
                        continue;
                    }
                    Err(ctx) => {
                        // Context is still processing, push it back
                        self.context.push(ctx);
                    }
                }
            }

            let pair = self.stack.pop()?;

            match pair.as_rule() {
                // End of input, we are done.
                Rule::EOI => {
                    return None;
                }
                // The program rule is the root of the path expression.
                Rule::program | Rule::path_binary_op => {
                    let inner_tokens = pair.into_inner().rev();
                    self.stack.extend(inner_tokens);
                }

                // * ==== [`ProcessingContext::PathExpr`] ==== //
                Rule::query => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::Query {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }

                // * ==== [`ProcessingContext::Filter`] ==== //
                Rule::filter_lens => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::Filter {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                // * ==== [`ProcessingContext::FilterOperation`] ==== //
                Rule::filter_operation => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::FilterOperation {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                Rule::filter_implicit_operation => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::FilterImplicitOperation {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }

                // * ==== [`ProcessingContext::Main`] ==== //
                // The main query rule is the final query expression.
                Rule::main_query => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::Main {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                // * ==== [`ProcessingContext::Group`] ==== //
                Rule::query_group => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::QueryGroup {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                // * ==== [`ProcessingContext::Group`] ==== //
                Rule::filter_group => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::Filter {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                // * ==== [`ProcessingContext::Molecule`] ==== //
                Rule::molecule => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::Molecule {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }
                // * ==== [`ProcessingContext::Definition`] ==== //
                Rule::lens_definition => {
                    // choice between path and filter
                    let pair = pair.into_inner().next().unwrap();
                    match pair.as_rule() {
                        Rule::query_lens_def => {
                            let mut inner = pair.clone().into_inner();
                            let name = inner.next().unwrap().into_inner().as_str().to_string();
                            let param_len = inner
                                .clone()
                                .filter(|p| p.as_rule() == Rule::lens_param)
                                .count();

                            self.delegate_to_context(pair, |remaining_pairs| {
                                ProcessingContext::QueryDefinition {
                                    lens: LensIdent(name.into()),
                                    params: Vec::new(),
                                    body: Query::default(),
                                    remaining_params: param_len,
                                    remaining_pairs, // remaining_pairs - 1 + expr_len,
                                    skip_first: true,
                                }
                            });
                        }
                        Rule::filter_lens_def => {
                            let mut inner = pair.clone().into_inner();
                            let name = inner.next().unwrap().into_inner().as_str().to_string();
                            let param_len = inner
                                .clone()
                                .filter(|p| p.as_rule() == Rule::lens_param)
                                .count();

                            self.delegate_to_context(pair, |remaining_pairs| {
                                ProcessingContext::FilterDefinition {
                                    lens: LensIdent(name.into()),
                                    params: Vec::new(),
                                    body: Filter(Vec::new()),
                                    remaining_params: param_len,
                                    remaining_pairs,
                                    skip_first: true,
                                }
                            });
                        }
                        _ => todo!("Lambda/Map lens definitions"),
                    }
                }
                // * ==== [`ProcessingContext::LensCall`] ==== //
                Rule::lens_call => {
                    let mut inner = pair.into_inner();
                    let name = inner.next().unwrap().into_inner().as_str().to_string().into();
                    let args_groups = inner
                        .map(|p| { p.into_inner().collect::<Vec<_>>() })
                        .collect::<Vec<_>>();
                    let total_pairs: usize = args_groups
                        .iter()
                        .map(|g| g.len())
                        .sum();

                    // Push all pairs to the main stack in reverse order
                    self.stack.extend(args_groups.into_iter().flat_map(|g| g.into_iter().rev()));

                    // Create a new context for the lens call
                    self.context.push(ProcessingContext::LensCall {
                        name,
                        args: Vec::new(),
                        remaining_args: total_pairs,
                    });
                }

                Rule::lens_param => {
                    let name = pair.into_inner().next().unwrap().into_inner().as_str().to_string();
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Lens(name.into())))
                    );
                }
                Rule::identifier => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Ident(pair.as_str().to_string())))
                    );
                }
                Rule::string => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::String(pair.as_str().to_string())))
                    );
                }
                Rule::int => {
                    let index = pair.as_str().parse::<i32>().unwrap();
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Index(index)))
                    );
                }
                Rule::lens_ident => {
                    let name = pair.into_inner().as_str().to_string().into();
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Lens(name)))
                    );
                }
                Rule::infix_lens => {
                    let name = pair.into_inner().next().unwrap().as_str().to_string().into();
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Infix(Infix::Lens(name)))
                    );
                }
                Rule::union => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Infix(Infix::Union))
                    );
                }
                Rule::intersect => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Infix(Infix::Intersection))
                    );
                }
                Rule::range_inclusive => {
                    let (start, end) = parse_range(pair, false);
                    self.handle_token_for_context(
                        Token::QueryToken(
                            QueryToken::Atom(Atom::Range(Range::Inclusive { start, end }))
                        )
                    );
                }
                Rule::range_exclusive => {
                    let (start, end) = parse_range(pair, false);
                    self.handle_token_for_context(
                        Token::QueryToken(
                            QueryToken::Atom(Atom::Range(Range::Exclusive { start, end }))
                        )
                    );
                }
                Rule::wildcard => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Wild(Wild::Once)))
                    );
                }
                Rule::wildcard_exact => {
                    let count = pair.into_inner().next().unwrap().as_str().parse::<u32>().unwrap();
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Atom(Atom::Wild(Wild::Exact(count))))
                    );
                }
                Rule::wildcard_range_exclusive => {
                    let is_implicit = pair.as_str().starts_with("*..");
                    let (start, end) = parse_range(pair, is_implicit);
                    self.handle_token_for_context(
                        Token::QueryToken(
                            QueryToken::Atom(
                                Atom::Wild(Wild::Range(Range::Exclusive { start, end }))
                            )
                        )
                    );
                }
                Rule::wildcard_range_inclusive => {
                    let is_implicit = pair.as_str().starts_with("*..=");
                    let (start, end) = parse_range(pair, is_implicit);
                    self.handle_token_for_context(
                        Token::QueryToken(
                            QueryToken::Atom(
                                Atom::Wild(Wild::Range(Range::Inclusive { start, end }))
                            )
                        )
                    );
                }
                // * ==== [`ProcessingContext::RecursiveWildcard`] ==== //
                Rule::wildcard_recursive => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::RecursiveWildcard {
                            until_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }

                Rule::optional => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Postfix(Postfix::Optional))
                    );
                }

                Rule::assertion => {
                    self.handle_token_for_context(
                        Token::QueryToken(QueryToken::Postfix(Postfix::Assertion))
                    );
                }

                Rule::filter_binary_op => {
                    let filter_op = match pair.as_str() {
                        "|" => FilterOperationToken::Infix(FilterInfix::Union),
                        "||" => FilterOperationToken::Infix(FilterInfix::Or),
                        "&&" => FilterOperationToken::Infix(FilterInfix::And),
                        "=" | "==" => FilterOperationToken::Infix(FilterInfix::Eq),
                        "!=" => FilterOperationToken::Infix(FilterInfix::Neq),
                        "<=" => FilterOperationToken::Infix(FilterInfix::Lte),
                        "<" => FilterOperationToken::Infix(FilterInfix::Lt),
                        ">=" => FilterOperationToken::Infix(FilterInfix::Gte),
                        ">" => FilterOperationToken::Infix(FilterInfix::Gt),
                        "add" => FilterOperationToken::Infix(FilterInfix::Add),
                        "sub" => FilterOperationToken::Infix(FilterInfix::Sub),
                        "mul" => FilterOperationToken::Infix(FilterInfix::Mul),
                        "div" => FilterOperationToken::Infix(FilterInfix::Div),
                        _ => unreachable!("Unexpected filter binary operation: {}", pair.as_str()),
                    };
                    self.handle_token_for_context(Token::FilterOperationToken(filter_op));
                }

                Rule::filter_postfix => {
                    panic!("filter_postfix is not supported yet: {:?}", pair);
                    let mut inner = pair.into_inner();

                    // Extract the first pair, which should be the mixfix.
                    let pair = inner.next().unwrap();
                    let name = pair.into_inner().next().unwrap().as_str().to_string().into();
                    let collected_tokens = vec![
                        FilterOperationToken::Infix(FilterInfix::Lens(name))
                    ];

                    let remaining_pairs = inner.len();

                    self.stack.extend(inner.rev());

                    // Handle the rest of the pairs in the context
                    self.context.push(ProcessingContext::FilterPostfix {
                        collected_tokens,
                        remaining_pairs,
                    });
                }

                Rule::query_postfix => {
                    self.delegate_to_context(pair, |remaining_pairs| {
                        ProcessingContext::PathPostfix {
                            collected_tokens: Vec::new(),
                            remaining_pairs,
                        }
                    });
                }

                Rule::float => {
                    let float_pair = pair.into_inner().next().unwrap();
                    let float_str = float_pair.as_str();

                    if float_str.ends_with("f32") {
                        let value = float_str
                            .trim_end_matches("f32")
                            .trim_end_matches("_")
                            .parse::<f32>()
                            .unwrap();
                        self.handle_token_for_context(
                            Token::QueryToken(QueryToken::Atom(Atom::Float(Float::F32(value))))
                        );
                    } else if float_str.ends_with("f64") {
                        let value = float_str
                            .trim_end_matches("f64")
                            .trim_end_matches("_")
                            .parse::<f64>()
                            .unwrap();
                        self.handle_token_for_context(
                            Token::QueryToken(QueryToken::Atom(Atom::Float(Float::F64(value))))
                        );
                    } else {
                        unreachable!();
                    }
                }
                Rule::neg => {
                    self.handle_token_for_context(
                        Token::FilterOperationToken(FilterOperationToken::Prefix(FilterPrefix::Neg))
                    );
                }
                Rule::not => {
                    self.handle_token_for_context(
                        Token::FilterOperationToken(FilterOperationToken::Prefix(FilterPrefix::Not))
                    );
                }

                _ => todo!("Unexpected rule in tokenization: {:?}", pair.as_rule()),
            }
        }
    }
}

impl<'input> Tokenizer<'input> {
    /// Create a new tokenizer from an input string.
    pub fn from_input(input: &'input str) -> Result<Self, TokenError> {
        Self::parse(Rule::program, input.trim()).map_err(TokenError::Parsing).map(Self::from_pairs)
    }

    /// Create a new tokenizer from a pairs iterator. [`Pairs`] are expected to be FRESH
    /// from the parser, meaning they have not been reversed.
    #[inline]
    #[must_use]
    fn from_pairs(pairs: Pairs<'input>) -> Self {
        Self { stack: pairs.rev().collect(), context: Vec::new(), pending: Vec::new() }
    }

    #[inline]
    fn delegate_to_context<F>(&mut self, pair: Pair<'input>, create: F)
        where F: FnOnce(usize) -> ProcessingContext
    {
        let inner_pairs = pair.into_inner().rev();
        let pair_len = inner_pairs.len();

        // Push the pairs in reverse order to the main stack
        self.stack.extend(inner_pairs);

        // Create a new context to collect tokens
        self.context.push(create(pair_len));
    }

    /// Handle a token based on the current processing context
    fn handle_token_for_context(&mut self, token: Token) {
        if let Some(context) = self.context.last_mut() {
            match context {
                ProcessingContext::Main { collected_tokens, remaining_pairs } => {
                    // collected_tokens.push(token);
                    push_token(token, collected_tokens);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::Query { collected_tokens, remaining_pairs } => {
                    push_token(token, collected_tokens);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::QueryGroup { collected_tokens, remaining_pairs } => {
                    push_as_query_token(token, collected_tokens);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::FilterGroup { collected_tokens, remaining_pairs } => {
                    let filter_token = FilterToken::try_from(token).expect(
                        "Expected a FilterToken in FilterGroup context"
                    );
                    collected_tokens.push(filter_token);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::Molecule { collected_tokens, remaining_pairs } => {
                    // if let Token::ExtendSuper(collected_tokens_from_context) = token {
                    //     collected_tokens.extend(collected_tokens_from_context);
                    // } else {
                    //     // If the token is not a delegate, just push it
                    //     collected_tokens.push(token);
                    // }
                    push_as_query_token(token, collected_tokens);

                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::LensCall { args, remaining_args, .. } => {
                    args.push(token);
                    *remaining_args = remaining_args.saturating_sub(1);
                }
                ProcessingContext::QueryDefinition {
                    params,
                    body,
                    remaining_pairs,
                    remaining_params,
                    skip_first: skip,
                    ..
                } => {
                    if *skip {
                        *remaining_pairs = remaining_pairs.saturating_sub(1);
                        *skip = false;
                        return;
                    }
                    match token {
                        Token::QueryToken(QueryToken::Atom(Atom::Lens(param))) if
                            *remaining_params > 0
                        => {
                            params.push(LensParam(param));
                        }
                        Token::Main(query) => {
                            // Unwrap the Main token and extend the body directly
                            body.0.extend(query.0);
                        }
                        token => {
                            push_as_query_token(token, &mut body.0);
                        }
                    }
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::FilterDefinition {
                    params,
                    body,
                    remaining_pairs,
                    remaining_params,
                    skip_first: skip,
                    ..
                } => {
                    if *skip {
                        *remaining_pairs = remaining_pairs.saturating_sub(1);
                        *skip = false;
                        return;
                    }
                    match token {
                        Token::QueryToken(QueryToken::Atom(Atom::Lens(param))) if
                            *remaining_params > 0
                        => {
                            params.push(LensParam(param));
                        }
                        _ => {
                            body.0.push(match token.try_into() {
                                Ok(filter_token) => filter_token,
                                Err(token) => {
                                    panic!(
                                        "Expected a FilterToken in FilterDefinition context found {:?}",
                                        token
                                    )
                                }
                            });
                        }
                    }
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::RecursiveWildcard { until_tokens, remaining_pairs } => {
                    let optional_len = remaining_pairs.saturating_sub(1);
                    let is_optional = optional_len > 0;
                    // until_tokens.push(token);

                    push_token(token, until_tokens);

                    if is_optional {
                        until_tokens.push(
                            Token::QueryToken(QueryToken::Postfix(Postfix::Optional))
                        );
                    }
                    *remaining_pairs = optional_len;
                }
                ProcessingContext::Filter { collected_tokens, remaining_pairs } => {
                    let filter_token = match token {
                        Token::FilterToken(filter_token) => filter_token,
                        Token::QueryToken(QueryToken::Filter(filter)) => FilterToken::Group(filter),
                        Token::QueryToken(QueryToken::Molecule(molecule)) =>
                            FilterToken::Molecule(molecule),
                        // Token::FilterOperationToken(filter_op_token) =>
                        //     FilterToken::Operation(FilterOperation(vec![filter_op_token])),
                        Token::FilterOperation(filter_op) => FilterToken::Operation(filter_op),
                        _ => unreachable!("Unexpected token in filter context: {:?}", token),
                    };

                    collected_tokens.push(filter_token);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::FilterOperation { collected_tokens, remaining_pairs } => {
                    match FilterOperationToken::try_from(token) {
                        Ok(filter_op_token) => {
                            collected_tokens.push(filter_op_token);
                        }
                        Err(token) => {
                            let Token::ExtendSuper(tokens) = token else {
                                unreachable!(
                                    "Expected a FilterOperationToken or ExtendSuper, got: {:?}",
                                    token
                                );
                            };

                            collected_tokens.extend(
                                tokens
                                    .into_iter()
                                    .map(FilterOperationToken::try_from)
                                    .collect::<Result<Vec<_>, _>>()
                                    .expect(
                                        "Expected FilterOperationTokens in FilterOperation context"
                                    )
                            );
                        }
                    }

                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::FilterImplicitOperation {
                    collected_tokens,
                    remaining_pairs,
                } => {
                    collected_tokens.push(token);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::FilterPostfix { collected_tokens, remaining_pairs } => {
                    let filter_op_token = FilterOperationToken::try_from(token).expect(
                        "Expected a FilterOperationToken in FilterPostfix context"
                    );
                    collected_tokens.push(filter_op_token);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
                ProcessingContext::PathPostfix { collected_tokens, remaining_pairs } => {
                    collected_tokens.push(token);
                    *remaining_pairs = remaining_pairs.saturating_sub(1);
                }
            }
        } else {
            // No active context, add to pending
            self.pending.push(token);
        }
    }

    /// Try to finalize a processing context if it's ready
    fn try_finalize_context(
        &mut self,
        context: ProcessingContext
    ) -> Result<Token, ProcessingContext> {
        match context {
            ProcessingContext::Main { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let mut query = Query::default();
                    for token in collected_tokens {
                        push_as_query_token(token, &mut query.0);
                    }

                    let token = Token::Main(query);
                    Ok(token)
                } else {
                    let context = ProcessingContext::Main {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::Query { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let token = Token::ExtendSuper(collected_tokens);
                    Ok(token)
                } else {
                    let context = ProcessingContext::Query {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::QueryGroup { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let token = Token::QueryToken(QueryToken::Group(collected_tokens.into()));
                    Ok(token)
                } else {
                    let context = ProcessingContext::QueryGroup {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::FilterGroup { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let token = Token::QueryToken(QueryToken::Filter(collected_tokens.into()));
                    Ok(token)
                } else {
                    let context = ProcessingContext::FilterGroup {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::Molecule { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let token = if collected_tokens.len() == 1 {
                        collected_tokens.into_iter().next().unwrap().into()
                    } else {
                        Token::QueryToken(QueryToken::Molecule(collected_tokens.into()))
                    };
                    Ok(token)
                } else {
                    let context = ProcessingContext::Molecule {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::LensCall { name, args, remaining_args, .. } => {
                if remaining_args == 0 {
                    let lens_call = LensCall {
                        lens: name.into(),
                        args,
                    };
                    let token = Token::QueryToken(QueryToken::Atom(Atom::Call(lens_call)));
                    Ok(token)
                } else {
                    let context = ProcessingContext::LensCall {
                        name,
                        args,
                        remaining_args,
                    };
                    Err(context)
                }
            }
            ProcessingContext::QueryDefinition {
                lens,
                params,
                body,
                remaining_pairs,
                remaining_params,
                skip_first,
            } => {
                if remaining_pairs == 0 {
                    let token = Token::Definition(Definition {
                        lens,
                        params,
                        body: body.into(),
                    });
                    Ok(token)
                } else {
                    let context = ProcessingContext::QueryDefinition {
                        lens,
                        params,
                        body,
                        remaining_pairs,
                        remaining_params,
                        skip_first,
                    };
                    Err(context)
                }
            }
            ProcessingContext::FilterDefinition {
                lens,
                params,
                body,
                remaining_pairs,
                remaining_params,
                skip_first,
            } => {
                if remaining_pairs == 0 {
                    let token = Token::Definition(Definition {
                        lens,
                        params,
                        body: body.into(),
                    });
                    Ok(token)
                } else {
                    let context = ProcessingContext::FilterDefinition {
                        lens,
                        params,
                        body,
                        remaining_pairs,
                        remaining_params,
                        skip_first,
                    };
                    Err(context)
                }
            }
            ProcessingContext::RecursiveWildcard { until_tokens, remaining_pairs } => {
                if remaining_pairs != 0 {
                    let context = ProcessingContext::RecursiveWildcard {
                        until_tokens,
                        remaining_pairs,
                    };
                    return Err(context);
                }

                let until_tokens: Vec<QueryToken> = until_tokens
                    .into_iter()
                    .map(|t| {
                        match t {
                            Token::QueryToken(query_token) => vec![query_token],
                            Token::ExtendSuper(extend) => {
                                extend
                                    .into_iter()
                                    .map(QueryToken::try_from)
                                    .collect::<Result<Vec<_>, _>>()
                                    .expect("Expected QueryTokens in ExtendSuper")
                            }
                            _ => unreachable!("Expected QueryToken or ExtendSuper, found: {:?}", t),
                        }
                    })
                    .flatten()
                    .collect();

                #[cfg(not(feature = "wild_until_last"))]
                let is_empty = until_tokens.is_empty();

                let token = Token::QueryToken(
                    QueryToken::Atom(Atom::Wild(Wild::Until(until_tokens.into())))
                );

                #[cfg(not(feature = "wild_until_last"))]
                if is_empty {
                    self.pending.push(Token::QueryToken(QueryToken::Atom(Atom::Wild(Wild::Once))));
                }

                Ok(token)
            }
            ProcessingContext::Filter { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    let token = Token::Filter(Filter(collected_tokens));
                    Ok(token)
                } else {
                    let context = ProcessingContext::Filter {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::FilterOperation { mut collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    // if is Implicit Query, and last is an infix, extract it out as postfix to upper context
                    match collected_tokens.last_mut() {
                        Some(FilterOperationToken::Implicit(ImplicitFilterToken::Query(query))) => {
                            if let Some(QueryToken::Infix(Infix::Lens(_))) = query.last() {
                                match query.pop().unwrap() {
                                    QueryToken::Infix(Infix::Lens(lens)) => {
                                        let token = FilterOperationToken::Postfix(
                                            FilterPostfix::InfixLens(lens)
                                        );

                                        collected_tokens.push(token);
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                        _ => {}
                    }

                    let token = Token::FilterOperation(FilterOperation(collected_tokens));
                    Ok(token)
                } else {
                    let context = ProcessingContext::FilterOperation {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::FilterImplicitOperation { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    assert_eq!(collected_tokens.len(), 1);
                    let token = collected_tokens.into_iter().next().unwrap();

                    Ok(token)
                } else {
                    let context = ProcessingContext::FilterImplicitOperation {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::FilterPostfix { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    Ok(Token::ExtendSuper(collected_tokens.into_iter().map(Token::from).collect()))
                } else {
                    let context = ProcessingContext::FilterPostfix {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
            ProcessingContext::PathPostfix { collected_tokens, remaining_pairs } => {
                if remaining_pairs == 0 {
                    Ok(Token::ExtendSuper(collected_tokens))
                } else {
                    let context = ProcessingContext::PathPostfix {
                        collected_tokens,
                        remaining_pairs,
                    };
                    Err(context)
                }
            }
        }
    }
}

/// Push a token into the collected tokens, handling `ExtendSuper` tokens by extending them
/// into the collected tokens.
fn push_token(token: Token, collected_tokens: &mut Vec<Token>) {
    if let Token::ExtendSuper(extend) = token {
        collected_tokens.extend(extend.into_iter().map(Token::from));
    } else {
        collected_tokens.push(token);
    }
}

fn extend_tokens(tokens: Vec<Token>, collected_tokens: &mut Vec<Token>) {
    for token in tokens {
        if let Token::ExtendSuper(extend) = token {
            collected_tokens.extend(extend.into_iter().map(Token::from));
        } else {
            collected_tokens.push(token);
        }
    }
}

fn push_as_query_token(token: Token, collected_tokens: &mut Vec<QueryToken>) {
    match token {
        Token::QueryToken(query_token) => collected_tokens.push(query_token),
        Token::ExtendSuper(extend) => {
            collected_tokens.extend(
                extend.into_iter().map(|t| {
                    match QueryToken::try_from(t) {
                        Ok(query_token) => query_token,
                        Err(t) =>
                            // If we encounter a token that is not a QueryToken, panic
                            // This should not happen in a well-formed query
                            unreachable!("Expected QueryToken in ExtendSuper, found: {:?}", t),
                    }
                })
            );
        }
        _ => unreachable!("Expected QueryToken or ExtendSuper, found: {:?}", token),
    }
}

impl TryFrom<Token> for ImplicitFilterToken {
    type Error = Token;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        match token {
            Token::QueryToken(QueryToken::Atom(Atom::Call(call))) => {
                Ok(ImplicitFilterToken::Call(call))
            }
            Token::QueryToken(QueryToken::Molecule(molecule)) => {
                Ok(ImplicitFilterToken::Query(Query::from(molecule)))
            }
            _ => Err(token),
        }
    }
}

#[inline]
fn parse_range(pair: Pair, is_implicit: bool) -> (u32, u32) {
    let mut inner = pair.into_inner();
    let start = if is_implicit {
        0
    } else {
        inner.next().unwrap().as_str().parse::<u32>().unwrap()
    };
    let end = inner.next().unwrap().as_str().parse::<u32>().unwrap();

    (start, end)
}

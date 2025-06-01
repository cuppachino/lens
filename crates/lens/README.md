# Lens

`lens` is a query engine for navigating structured data using composable, Rust-inspired path expressions.

This crate implements the foundational query syntax for traversing nested data structures and is designed to serve as a foundation for higher-level tooling and data manipulation systems.

> **Status**: This implementation is currently in active development. APIs and behavior are subject to change as the project evolves toward stability.

## Problem Statement

Modern data processing requires working with multiple formats, each with its own query paradigm: JSONPath for JSON, XPath for XML, SQL for relational databases. This fragmentation creates cognitive overhead and limits interoperability between systems.

## Design Goals

- **Unified Query Language**: One unified syntax capable of querying any structured data format including JSON, TOML, XML, Markdown, etc.
- **Intuitive Syntax**: Language-inspired expressions rather than cryptic pattern matching
- **Composable Operations**: Declarative query composition enabling complex data navigation
- **Extensible Foundation**: Core primitives for building patching, linting, schema validation, and transformation tools

### Status

This crate is **pre-alpha**. Many features are stubbed out or only partially implemented. The syntax is still evolving and may change.

## Architecture Overview

### Query Composition Model

The query system is built around a hierarchical composition model:

- **Atoms**: Fundamental query units (identifiers, wildcards, ranges)
- **Molecules**: Sequences of atoms connected by path operators (`.`)
- **Queries**: Collections of molecules combined with logical operators

### Filter Operations

Filters apply conditional logic to data contexts. They support logical operators and implicit value validation.

### Lens Abstraction

The lens system treats molecules as first-class values by default. To access [magic](<https://en.wikipedia.org/wiki/Magic_number_(programming)>) data values rather than query expressions, special syntax is required. Rather than introducing new syntax, the system reuses the existing lens notation to represent literal values:

| Data Type | Lens Syntax       |
| --------- | ----------------- |
| Integers  | `'0`, `'1`, `'42` |
| Booleans  | `'true`, `'false` |
| Null      | `'null`           |

This approach maintains syntactic consistency while clearly distinguishing between query expressions and literal values.

## Query Language Reference

| Expression Type    | Syntax                   | Description                          | Implementation Status |
| ------------------ | ------------------------ | ------------------------------------ | --------------------- |
| Quoted Identifiers | `'field'`, `"field"`     | Field access with special characters | ✅                    |
| Path Traversal     | `a.b.c`                  | Nested property navigation           | ✅                    |
| Optional Access    | `field?`                 | Null-safe field access               | ✅                    |
| Array Indexing     | `0`, `1`, `-1`           | Element access by position           | ✅                    |
| Range Slicing      | `0..3`, `1..=5`          | Subsequence extraction               | ✅                    |
| Wildcards          | `*`                      | Match any single element             | ✅                    |
| Counted Wildcards  | `*4`, `*2..=5`           | Match specific quantities            | ✅                    |
| Recursive Descent  | `**.field`               | Deep search across structure         | ✅                    |
| Union Operations   | `name\|title\|label`     | Logical OR                           | ✅                    |
| Intersection       | `required&valid`         | Logical AND                          | ✅                    |
| Optional Operator  | `path?`                  | Fallible atom                        | ✅                    |
| Assertion Operator | `required!`              | Enforce atom existence               | ✅                    |
| Grouping           | `(expression)`           | Precedence control                   | ✅                    |
| Filtering          | `~{ condition }`         | Conditional data selection           | ✅                    |
| Mapping            | `->{ transformation }`   | Structure transformation             | 🚧                    |
| Named Query Lens   | `'alias: lens`           | Reusable query definitions           | ✅                    |
| Named Filter Lens  | `'alias:~{ condition }`  | Reusable filter definitions          | ✅                    |
| Named Lambda Lens  | `'alias:->{ mapping }`   | Reusable transformations             | 🚧                    |
| Infix Operations   | `` lhs `op` rhs ``       | Custom binary operators              | ✅                    |
| Float Literals     | `'3.14f64`, `'1.0e2_f32` | Lenses that yield a constant float   | ✅                    |

## Usage

The current implementation provides parsing and query iteration capabilities. **Data model integration is the responsibility of the implementing application.**

> **Note**: The tokenization layer is complete, but AST construction and evaluation remain in development. The crate is not yet suitable for production query execution.

### Integration Pattern

1. **Parse**: Convert query strings into `Expr` token streams using the provided parser
2. **Navigate**: Implement domain-specific logic to interpret `Expr` operations over your data model (e.g., `serde_json::Value`)
3. **Execute**: Handle advanced operations including:
   - Context binding and variable resolution
   - Filter evaluation (`~{ condition }`)
   - Post-filter projections
   - Aggregation and mutation operations

### Architecture Layers

- **Parser**: Provided by the `lens` crate
- **Navigator**: Application-specific data traversal logic
- **Executor**: Application-specific operation evaluation (required for full query support)

### Design Philosophy

The goal is to keep the core logic minimal and embeddable. Higher-level crates (e.g., `lens-json`, `lens-navigator`) can wrap this with easier APIs and common data backends later.

## Roadmap

### Phase 1: Core Stability

- [ ] Complete CST and stabilize language syntax
- [ ] Complete AST and stabilize intermediate representation
- [ ] JSON navigator and executor implementation
- [ ] Comprehensive test suite and documentation

### Phase 2: Ecosystem Development

- [ ] IDE integration and syntax highlighting

### Phase 3: Format Expansion

- [ ] XML, TOML, and YAML navigator implementations
- [ ] Mutation operations and patch support
- [ ] Query optimization and performance profiling

## Contributing

This project is in active development and your ideas are invaluable. Here are some ways you can help:

- **API Feedback**: Test the query syntax against your use cases and report friction points
- **Use Case Documentation**: Share specific navigation requirements from your domain
- **Implementation Contributions**: Help develop navigators for additional data formats
- **Testing**: Provide edge cases and real-world query patterns for validation

For substantial contributions or architectural discussions, please open an issue before beginning implementation work.

## License

MIT License (MIT)

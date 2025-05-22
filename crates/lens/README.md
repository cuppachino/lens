# Lens

`lens` is a work-in-progress query engine for navigating structured data using composable, Rust-inspired path expressions.

This crate implements the base query syntax for traversing nested data. It is intended to serve as a foundation for higher-level tooling.

> Note: The implementation is **incomplete** and **unstable**. APIs and behavior may change frequently.

**The Problem**: Every data format has its own query syntax. JSONPath for JSON, XPath for XML, SQL for databases—each with different capabilities and syntax quirks.

## Goals

- One unified syntax to query **any structured data**: JSON, TOML, XML, Markdown, databases (SQL transpilation), etc.
- Familiar like a programming language, not cryptic regex patterns.
- Declarative query composition
- Foundation for patching, linting, schema validation, etc.

## Status

This crate is **pre-alpha**. Many features are stubbed out or only partially implemented. The syntax is still evolving and may change.

## Query Syntax

The query language combines path expressions, pattern matching, and filtering:

| Expression           | Description        | Example                       | Status |
| -------------------- | ------------------ | ----------------------------- | ------ |
| `'field'`, `"field"` | Quoted identifiers | `"user-name"`, `'first name'` | ✅     |
| `a.b`                | Property traversal | `user.profile.name`           | ✅     |
| `a?`                 | Optional field     | `metadata?.title`             | ✅     |
| `0`, `1`             | Array indexing     | `users.0.name`                | ✅     |
| `0..3`, `1..=5`      | Range slicing      | `items.0..10`, `users.5..=15` | ✅     |
| `*`                  | Single wildcard    | `users.*.name`                | ✅     |
| `*4`, `*2..=5`       | Counted wildcards  | `data.*3.value`               | ✅     |
| `**.field`           | Recursive search   | `**.email` (find all emails)  | ✅     |
| `a\|b\|c`            | Union (OR)         | `name\|title\|label`          | ✅     |
| `a&b`                | Intersection (AND) | `required&valid`              | 🚧     |
| `(...)`              | Grouping           | `(name\|title)&required`      | ✅     |
| `'alias: lens`       | Named binding      | `'users: **.user`             | 🚧     |
| `~{ condition }`     | Filtering          | `users~{ age > 18 }`          | 🚧     |

## Usage

At the moment, `lens` provides parsing and iteration over the query syntax — but **you are responsible** for wiring it to a data model (e.g., JSON) yourself.

To evaluate a query, you might:

1. Parse the query string into an iterator of `Expr::Lens` items:

   ```rust
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
   ```

2. Implement a **navigator** that interprets each `Expr` step over your data model (like `serde_json::Value`), yielding intermediate matches.

3. Optionally, build an **executor** to handle advanced operations like:

   - context joins (e.g., binding intermediate results)
   - pipes and filters (`~{ condition }`)
   - projections (`.field` after a filter)
   - aggregations or mutations

This means you'll need a few layers:

- Parser: already provided (`LensesIter`)
- Navigator: your logic
- Executor: your logic (optional, but needed for full queries)

If you're working with JSON, deserialize to a `Value` and write a custom navigator over that. The crate doesn’t provide one yet.

### Why it's like this

The goal is to keep the core logic minimal and embeddable. Higher-level crates (e.g., `lens-json`, `lens-navigator`) can wrap this with easier APIs and common data backends later.

---

## Example Use Cases

- Traverse nested records or arrays
- Filter nodes based on value conditions
- Compose multiple lenses to extract, transform, or patch data

## Roadmap

### Near-term

- [ ] Intersection operators (`a&b`)
- [ ] Named bindings for query reuse
- [ ] Complete filter syntax (`~{ condition }`)
- [ ] JSON navigator and executor implementation

### Medium-term

- [ ] Type-safe query construction API
- [ ] Navigators for XML, TOML, YAML
- [ ] Mutation support (patch operations)
- [ ] Performance optimizations

### Long-term

- [ ] SQL transpilation for database queries
- [ ] IDE tooling and syntax highlighting

## Contributing

This project is in active development and your ideas are invaluable. Here are some ways you can help:

- **Feedback**: Try the syntax with your data and share what works/doesn't
- **Use cases**: Tell us about your specific navigation needs
- **Implementation**: Help build navigators for different data formats
- **Testing**: Edge cases and real-world query patterns

If you're building something on top of this (like a UI navigator or patch tool), feel free to open issues or discussions.

## License

MIT License (MIT)

---

_Lens: Make data navigation feel like writing code, not solving riddles._

pub mod error;
pub mod token;
pub mod tokenizer;

/// `use lens::tokens::prelude::*;` to import commonly used items.
pub mod prelude {
    pub use super::{ token::{ *, filter::*, hs::* }, tokenizer::{ Tokenizer } };
}

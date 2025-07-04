pub mod errors;
pub mod variant;
pub use variant::{Value, ValueType, Variant, convert, register_conversion};
mod tests;
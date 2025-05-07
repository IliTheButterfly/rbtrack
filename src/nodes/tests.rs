use super::system::Variant;
use std::any::TypeId;
use crate::{register_value_type, register_conversion, impl_value_conversion};
use super::system::*;

#[cfg(test)]
#[test]
fn it_works() {

    register_conversion!(String => i32,
        |v: String | v.parse::<i32>().unwrap_or_default());
    register_conversion!(i32 => i32,
        |v: i32 | v);
    
    let var: Value = 15.into();
    assert_eq!(var.as_int(), Some(15).as_ref());
    assert_eq!(var.as_int(), Some(15).as_ref());
    let var: Option<Value> = convert(&var, TypeId::of::<i32>());
    assert_eq!(var.unwrap().as_int(), Some(15).as_ref());
    
    let var: Value = Value::String("20".to_string());
    assert_eq!(var.as_string(), Some("20".to_string()).as_ref());
    let var: Option<Value> = convert(&var, TypeId::of::<i32>());
    assert_eq!(var.unwrap().as_int(), Some(20).as_ref());
}
use std::any::TypeId;
use crate::{register_value_type, register_conversion, impl_value_conversion};
use super::errors::*;
use super::variant::*;
use paste::paste;

#[cfg(feature = "opencv-types")]
use opencv::core::{self as cv, CV_8U};

#[cfg(test)]
#[test]
fn simple_conversion() {

    register_conversion!(String => i32,
        |v: String | v.parse::<i32>().unwrap_or_default());
    register_conversion!(i32 => i32,
        |v: i32 | v);
    
    let var: Value = 15.into();
    assert_eq!(var.as_int(), Some(&15));
    assert_eq!(var.as_int(), Some(&15));
    let var: Option<Value> = convert(&var, TypeId::of::<i32>());
    assert_eq!(var.unwrap().as_int(), Some(&15));
    
    let var: Value = Value::String("20".to_string());
    assert_eq!(var.as_string(), Some(&"20".to_string()));
    let var: Option<Value> = convert(&var, TypeId::of::<i32>());
    assert_eq!(var.unwrap().as_int(), Some(&20));
}

macro_rules! mat_into_tests {
    (imp {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        paste! {
            #[test]
            fn [<mat_into_ $variant>] () {
                let value: Value = unsafe { cv::Mat::new_nd(&[$($dims),+], $cv_t) }.unwrap().into();
                assert_ne!(value.inner_type_id(), TypeId::of::<cv::Mat>());
                assert_eq!(value.inner_type_id(), TypeId::of::<$t>());
            }
        } 
    };
    ({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        mat_into_tests!(imp {$t, $variant, $cv_t, $($dims),+});
        mat_into_tests!($({$t2, $variant2, $cv_t2, $($dims2),+}), +);
    };
    ({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        mat_into_tests!(imp {$t, $variant, $cv_t, $($dims),+});
    };
}

mat_into_tests!(
    { cv::Mat1b, Vector1B, cv::CV_8U, 1 },
    { cv::Mat2b, Vector2B, cv::CV_8U, 2 },
    { cv::Mat3b, Vector3B, cv::CV_8U, 3 },
    { cv::Mat4b, Vector4B, cv::CV_8U, 4 },
    { cv::Mat1w, Vector1W, cv::CV_16U, 1 },
    { cv::Mat2w, Vector2W, cv::CV_16U, 2 },
    { cv::Mat3w, Vector3W, cv::CV_16U, 3 },
    { cv::Mat4w, Vector4W, cv::CV_16U, 4 },
    { cv::Mat1i, Vector1I, cv::CV_32S, 1 },
    { cv::Mat2i, Vector2I, cv::CV_32S, 2 },
    { cv::Mat3i, Vector3I, cv::CV_32S, 3 },
    { cv::Mat4i, Vector4I, cv::CV_32S, 4 },
    { cv::Mat1f, Vector1F, cv::CV_32F, 1 },
    { cv::Mat2f, Vector2F, cv::CV_32F, 2 },
    { cv::Mat3f, Vector3F, cv::CV_32F, 3 },
    { cv::Mat4f, Vector4F, cv::CV_32F, 4 }
);

#[test]
fn variant_value_conversions() {
    let vars: Vec<Value> = vec![5.into(), 6.into()];
    let batch: Variant = vars.into();

    assert!(batch.is_vector());
    let vars = batch.as_vector().unwrap();
    assert_eq!(vars[0].as_int(), Some(&5));
    assert_eq!(vars[1].as_int(), Some(&6));

    let vars: Vec<Value> = batch.try_into().unwrap();
    assert_eq!(vars[0].as_int(), Some(&5));
    assert_eq!(vars[1].as_int(), Some(&6));

    let batch: Variant = vars.into();
    let vars: Vec<i32> = batch.try_into().unwrap();
    assert_eq!(vars[0], 5);
    assert_eq!(vars[1], 6);

    let batch: Variant = vars.into();
    let vars = batch.as_vector().unwrap();
    assert_eq!(vars[0].as_int(), Some(&5));
    assert_eq!(vars[1].as_int(), Some(&6));
}
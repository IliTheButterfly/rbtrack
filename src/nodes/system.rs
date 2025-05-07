use once_cell::sync::Lazy;
use uuid::Uuid;
use variation::Variation;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::RwLock;
use opencv::prelude::MatTraitConst;
use opencv::prelude::MatSizeTraitConst;
use crate::common::errors::BTrackError;

#[derive(Clone, Debug)]
enum PortType {
    Input,
    Output,
}
















#[derive(Variation)]
pub enum Value {
    Boolean(bool),
    Int(i32),
    Float(f32),
    String(String),
    UUID(Uuid),
    Vector1B(opencv::core::Mat),
    Vector2B(opencv::core::Mat),
    Vector3B(opencv::core::Mat),
    Vector4B(opencv::core::Mat),
    Vector1I(opencv::core::Mat),
    Vector2I(opencv::core::Mat),
    Vector3I(opencv::core::Mat),
    Vector4I(opencv::core::Mat),
    Vector1F(opencv::core::Mat),
    Vector2F(opencv::core::Mat),
    Vector3F(opencv::core::Mat),
    Vector4F(opencv::core::Mat),
    Rect2I(opencv::core::Rect2i),
    Rect2F(opencv::core::Rect2f),
    Size2I(opencv::core::Size2i),
    Size2F(opencv::core::Size2f),
    Mat(opencv::core::Mat),
    Custom(Box<dyn Any + Send + Sync>),
}

impl Value {
}

type ConversionFn = Box<dyn Fn(&Value) -> Option<Value> + Send + Sync>;

static CONVERSIONS: Lazy<RwLock<HashMap<(TypeId, TypeId), ConversionFn>>> = Lazy::new(|| {
    RwLock::new(HashMap::new())
});

pub trait ValueType: Send + Sync + 'static {
    fn type_id(&self) -> TypeId;
    fn to_value(&self) -> Value;
    fn from_value(value: &Value) -> Option<Self> where Self: Sized;
}

pub fn register_conversion<F>(from: TypeId, to: TypeId, func: F)
where
    F: Fn(&Value) -> Option<Value> + Send + Sync + 'static,
{
    CONVERSIONS
        .write()
        .unwrap()
        .insert((from, to), Box::new(func));
}

pub fn convert(value: &Value, to: TypeId) -> Option<Value> {
    let from = match value {
        Value::Boolean(_) => TypeId::of::<bool>(),
        Value::Int(_) => TypeId::of::<i32>(),
        Value::Float(_) => TypeId::of::<f32>(),
        Value::String(_) => TypeId::of::<String>(),
        Value::UUID(_) => TypeId::of::<Uuid>(),
        Value::Vector1B(_) => TypeId::of::<opencv::core::Mat1b>(),
        Value::Vector2B(_) => TypeId::of::<opencv::core::Mat2b>(),
        Value::Vector3B(_) => TypeId::of::<opencv::core::Mat3b>(),
        Value::Vector4B(_) => TypeId::of::<opencv::core::Mat4b>(),
        Value::Vector1I(_) => TypeId::of::<opencv::core::Mat1i>(),
        Value::Vector2I(_) => TypeId::of::<opencv::core::Mat2i>(),
        Value::Vector3I(_) => TypeId::of::<opencv::core::Mat3i>(),
        Value::Vector4I(_) => TypeId::of::<opencv::core::Mat4i>(),
        Value::Vector1F(_) => TypeId::of::<opencv::core::Mat1f>(),
        Value::Vector2F(_) => TypeId::of::<opencv::core::Mat2f>(),
        Value::Vector3F(_) => TypeId::of::<opencv::core::Mat3f>(),
        Value::Vector4F(_) => TypeId::of::<opencv::core::Mat4f>(),
        Value::Rect2I(_) => TypeId::of::<opencv::core::Rect2i>(),
        Value::Rect2F(_) => TypeId::of::<opencv::core::Rect2f>(),
        Value::Size2I(_) => TypeId::of::<opencv::core::Size2i>(),
        Value::Size2F(_) => TypeId::of::<opencv::core::Size2f>(),
        Value::Mat(_) => TypeId::of::<opencv::core::Mat>(),
        Value::Custom(b) => (*b).type_id(),
    };
    

    CONVERSIONS.read().unwrap().get(&(from, to)).and_then(|f| f(value))
}

#[derive(Variation)]
pub enum Variant {
    Single(Value),
    Vector(Vec<Value>),
}

#[macro_export]
macro_rules! impl_value_conversion {
    ($t:ty, $variant:ident) => {
        impl TryFrom<&Value> for $t {
            type Error = &'static str;
            fn try_from(v: &Value) -> Result<Self, Self::Error> {
                match v {
                    Value::$variant(inner) => Ok(*inner),
                    _ => Err("Type mismatch"),
                }
            }
        }

        impl From<$t> for Value {
            fn from(v: $t) -> Self {
                Value::$variant(v)
            }
        }
    };

    // For custom boxed types
    (custom $t:ty) => {
        impl TryFrom<&Value> for &$t {
            type Error = &'static str;
            fn try_from(v: &Value) -> Result<Self, Self::Error> {
                if let Value::Custom(b) = v {
                    b.downcast_ref::<$t>().ok_or("Type mismatch")
                } else {
                    Err("Type mismatch")
                }
            }
        }

        impl From<$t> for Value {
            fn from(v: $t) -> Self {
                Value::Custom(Box::new(v))
            }
        }
    };
}


#[macro_export]
macro_rules! register_value_type {
    ($t:ty, $variant:ident) => {
        impl ValueType for $t {
            fn type_id(&self) -> TypeId { TypeId::of::<$t>() }
            fn to_value(&self) -> Value { Value::$variant(self.clone()) }
            fn from_value(value: &Value) -> Option<Self> {
                if let Value::$variant(v) = value {
                    Some(v.clone())
                } else {
                    None
                }
            }
        }
    };
}

macro_rules! register_mat {
    (dim $actual:ident, $($dims:literal),+) => (register_mat!(dim_mid $actual, 0, $($dims),+));
    (dim_mid $actual:ident, $index:expr, $dims:literal) => ($actual.dims() > $index && $actual.get($index).unwrap() == $dims);
    (dim_mid $actual:ident, $index:expr, $dims:literal, $($dims2:literal),+) => {
        register_mat!(dim_mid $actual, $index, $dims) && register_mat!(dim_mid $actual, $index + 1, $($dims2),+)
    };

    (typ_id $({$t:ty, $variant:ident, $($dims:literal),+}),+) => {
        fn type_id(&self) -> TypeId {
            let actual = self.mat_size();
            register_mat!(typ_id_mid actual, $({$t, $variant, $($dims),+}),+);
            TypeId::of::<opencv::core::Mat>()
        }
    };
    (typ_id_impl $actual:ident, $t:ty, $variant:ident, $($dims:literal),+) => {
        if register_mat!(dim $actual, $($dims),+) {
            return TypeId::of::<$t>();
        }
    };
    (typ_id_mid $actual:ident, {$t:ty, $variant:ident, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $($dims2:literal),+}),+) => {
        register_mat!(typ_id_impl $actual, $t, $variant, $($dims), +);
        register_mat!(typ_id_mid $actual, $({$t2, $variant2, $($dims2), +}), +)
    };
    (typ_id_mid $actual:ident, {$t:ty, $variant:ident, $($dims:literal),+}) => {
        register_mat!(typ_id_impl $actual, $t, $variant, $($dims), +)
    };

    (to_val {$t:ty, $variant:ident, $($dims:literal),+}) => {
        fn to_value(&self) -> Value {
            register_mat!(to_val_mid $({$t, $variant, $($dims),+}),+)
        }
    };
    (to_val_impl {$t:ty, $variant:ident, $($dims:literal),+}) => {
        Value::$variant(self.clone())
    };
    (to_val_mid {$t:ty, $variant:ident, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $($dims2:literal),+}),+) => {
        register_mat!(to_val_impl, {$t, $variant, $($dims), +})
        register_mat!(to_val_mid $({$t2, $variant2, $($dims2), +}), +)
    };
    (to_val_mid {$t:ty, $variant:ident, $($dims:literal),+}) => {
        register_mat!(to_val_impl, {$t, $variant, $($dims), +})
    };

    (from_val {$t:ty, $variant:ident, $($dims:literal),+}) => {
        fn from_value(value: &Value) -> Option<Self> {
            register_mat!(from_val_mid $({$t, $variant, $($dims),+}),+)
        }
    };
    (from_val_impl {$t:ty, $variant:ident, $($dims:literal),+}) => {
        if let Value::$variant(v) = value {
            Some(v.clone())
        } else {
            None
        }
    };
    (from_val_mid {$t:ty, $variant:ident, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $($dims2:literal),+}),+) => {
        register_mat!(from_val_impl, {$t, $variant, $($dims), +});
        register_mat!(from_val_mid $({$t2, $variant2, $($dims2), +}), +)
    };
    (from_val_mid {$t:ty, $variant:ident, $($dims:literal),+}) => {
        register_mat!(from_val_impl, {$t, $variant, $($dims), +})
    };

    ($({$t:ty, $variant:ident, $($dims:literal),+}),+) => {
        impl ValueType for opencv::core::Mat {
            register_mat!(typ_id $({$t, $variant, $($dims), +}), +);
            fn from_value(value: &Value) -> Option<Self> {
                None
            }
            fn to_value(&self) -> Value {
                Value::Int(1)
            }
            // register_mat!(from_val $({$t, $variant, $($dims), +}), +)
            // register_mat!(to_val $({$t, $variant, $($dims), +}), +)
        }
    };
}

// impl ValueType for opencv::core::Mat {
//     fn type_id(&self) -> TypeId { TypeId::of::<$t>() }
//     fn to_value(&self) -> Value { 
//         let mut ret = opencv::core::Mat::zeros(self.cols(), self.rows(), self.typ());
//         Value::$variant(ret)
//     }
//     fn from_value(value: &Value) -> Option<Self> {
//         if let Value::$variant(v) = value {
//             Some(v.clone())
//         } else {
//             None
//         }
//     }
// }

#[macro_export]
macro_rules! register_conversion {
    ($from:ty => $to:ty, $func:expr) => {{
        $crate::nodes::system::register_conversion(
            TypeId::of::<$from>(),
            TypeId::of::<$to>(),
            |val| {
                if let Some(v) = <$from as ValueType>::from_value(val) {
                    let out: $to = $func(v);
                    Some(out.to_value())
                } else {
                    None
                }
            }
        );
    }};
}

register_value_type!(bool, Boolean);
register_value_type!(i32, Int);
register_value_type!(f32, Float);
register_value_type!(String, String);
register_value_type!(Uuid, UUID);

register_mat!(
    { opencv::core::Mat1b, Vector1B, 1 },
    { opencv::core::Mat2b, Vector2B, 2 },
    { opencv::core::Mat3b, Vector3B, 3 },
    { opencv::core::Mat4b, Vector4B, 4 },
    { opencv::core::Mat1i, Vector1I, 1 },
    { opencv::core::Mat2i, Vector2I, 2 },
    { opencv::core::Mat3i, Vector3I, 3 },
    { opencv::core::Mat4i, Vector4I, 4 },
    { opencv::core::Mat1f, Vector1F, 1 },
    { opencv::core::Mat2f, Vector2F, 2 },
    { opencv::core::Mat3f, Vector3F, 3 },
    { opencv::core::Mat4f, Vector4F, 4 }
);
register_value_type!(opencv::core::Rect2i, Rect2I);
register_value_type!(opencv::core::Rect2f, Rect2F);
register_value_type!(opencv::core::Size2i, Size2I);
register_value_type!(opencv::core::Size2f, Size2F);
// register_value_type!(opencv::core::Mat, Mat);

impl_value_conversion!(bool, Boolean);
impl_value_conversion!(i32, Int);
impl_value_conversion!(f32, Float);
impl_value_conversion!(Uuid, UUID);

fn base_conversions() {
    // Self conversions
    register_conversion!(bool => bool,
        |v: bool | v);
    register_conversion!(i32 => i32,
        |v: i32 | v);
    register_conversion!(f32 => f32,
        |v: f32 | v);
    register_conversion!(String => String,
        |v: String | v.clone());
    register_conversion!(Uuid => Uuid,
        |v: Uuid | v);
    register_conversion!(opencv::core::Mat => opencv::core::Mat,
        |v: opencv::core::Mat | v.clone());

    // String conversions
    register_conversion!(String => Uuid,
        |v: String | Uuid::try_parse(&v[..]).unwrap_or_default());
    register_conversion!(String => bool,
        |v: String | v.parse::<bool>().unwrap_or_default());
    register_conversion!(String => f32,
        |v: String | v.parse::<f32>().unwrap_or_default());
    register_conversion!(String => i32,
        |v: String | v.parse::<i32>().unwrap_or_default());
    
    // Number conversions
    register_conversion!(bool => i32, |v: bool| i32::from(v));
    register_conversion!(bool => f32, |v: bool| f32::from(v));
    register_conversion!(i32 => bool, |v: i32| v > 0);
    register_conversion!(i32 => f32, |v: i32| v as f32);
    register_conversion!(f32 => bool, |v: f32| v > 0.5);
    register_conversion!(f32 => i32, |v: f32| unsafe { v.round().to_int_unchecked() });
}

enum PortValueDimensions {
    // Assume there is only one value. Clone when dispatching to threads.
    Single,
    // Assume there are multiple values. 
    // Dispatch into n threads, each thread getting their respective value.
    Array,
}

pub trait Item {
    fn label(&self) -> &str;
    fn label_mut(&mut self) -> &mut str;
    fn desc(&self) -> &str;
    fn desc_mut(&mut self) -> &mut str;
    fn id(&self) -> &Uuid;
    fn id_mut(&mut self) -> &mut Uuid;
    fn parent_id(&self) -> &Option<Uuid>;
    fn parent_id_mut(&mut self) -> &mut Option<Uuid>;
    fn clone_item(&self) -> Self;
}

pub trait Node: Send + Item {
    fn run(&mut self) -> Result<(), BTrackError>;
    fn compile(&mut self) -> Result<(), BTrackError>;
}

pub trait Port: Send + Item {
}


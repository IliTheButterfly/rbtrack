use once_cell::sync::Lazy;
use uuid::Uuid;
use variation::Variation;
use std::any::{Any, TypeId};
use graphile_worker_extensions::AnyClone;
use std::collections::HashMap;
use parking_lot::RwLock;
use anyhow::anyhow;

#[cfg(feature = "opencv-types")]
use opencv::prelude::{MatTraitConst,MatSizeTraitConst};


#[derive(Variation, Clone)]
pub enum Value {
    // Base types
    Boolean(bool),
    Word(u16),
    Int(i32),
    Float(f32),
    String(String),
    UUID(Uuid),

    // Points
    #[cfg(feature = "opencv-types")]
    Point2I(opencv::core::Point2i),
    #[cfg(feature = "opencv-types")]
    Point2F(opencv::core::Point2f),
    #[cfg(feature = "opencv-types")]
    Point3I(opencv::core::Point3i),
    #[cfg(feature = "opencv-types")]
    Point3F(opencv::core::Point3f),

    // Vectors
    #[cfg(feature = "opencv-types")]
    Vector1B(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector2B(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector3B(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector4B(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector1W(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector2W(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector3W(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector4W(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector1I(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector2I(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector3I(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector4I(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector1F(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector2F(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector3F(opencv::core::Mat),
    #[cfg(feature = "opencv-types")]
    Vector4F(opencv::core::Mat),

    // Matrices
    #[cfg(feature = "opencv-types")]
    Mat44F(opencv::core::Matx44f),
    #[cfg(feature = "opencv-types")]
    Mat33F(opencv::core::Matx33f),

    // Any other matrix
    #[cfg(feature = "opencv-types")]
    Mat(opencv::core::Mat),

    // Rects
    #[cfg(feature = "opencv-types")]
    Rect2I(opencv::core::Rect2i),
    #[cfg(feature = "opencv-types")]
    Rect2F(opencv::core::Rect2f),

    // Sizes
    #[cfg(feature = "opencv-types")]
    Size2I(opencv::core::Size2i),
    #[cfg(feature = "opencv-types")]
    Size2F(opencv::core::Size2f),

    // Recursion
    Variant(Box<Variant>),

    // Custom
    Custom(Box<dyn AnyClone + Send + Sync>),
}

impl Value {
    #[cfg(feature = "opencv-types")]
    pub const Transform4F: fn(opencv::core::Matx44f) -> Value = Value::Mat44F;

    pub fn inner_type_id(&self) -> TypeId {
        match self {
            Value::Boolean(_) => TypeId::of::<bool>(),
            Value::Word(_) => TypeId::of::<u16>(),
            Value::Int(_) => TypeId::of::<i32>(),
            Value::Float(_) => TypeId::of::<f32>(),
            Value::String(_) => TypeId::of::<String>(),
            Value::UUID(_) => TypeId::of::<Uuid>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Point2I(_) => TypeId::of::<opencv::core::Point2i>(),
            #[cfg(feature = "opencv-types")]
            Value::Point2F(_) => TypeId::of::<opencv::core::Point2f>(),
            #[cfg(feature = "opencv-types")]
            Value::Point3I(_) => TypeId::of::<opencv::core::Point3i>(),
            #[cfg(feature = "opencv-types")]
            Value::Point3F(_) => TypeId::of::<opencv::core::Point3f>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Vector1B(_) => TypeId::of::<opencv::core::Mat1b>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector2B(_) => TypeId::of::<opencv::core::Mat2b>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector3B(_) => TypeId::of::<opencv::core::Mat3b>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector1W(_) => TypeId::of::<opencv::core::Mat1w>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector2W(_) => TypeId::of::<opencv::core::Mat2w>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector3W(_) => TypeId::of::<opencv::core::Mat3w>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector4W(_) => TypeId::of::<opencv::core::Mat4w>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector4B(_) => TypeId::of::<opencv::core::Mat4b>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector1I(_) => TypeId::of::<opencv::core::Mat1i>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector2I(_) => TypeId::of::<opencv::core::Mat2i>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector3I(_) => TypeId::of::<opencv::core::Mat3i>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector4I(_) => TypeId::of::<opencv::core::Mat4i>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector1F(_) => TypeId::of::<opencv::core::Mat1f>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector2F(_) => TypeId::of::<opencv::core::Mat2f>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector3F(_) => TypeId::of::<opencv::core::Mat3f>(),
            #[cfg(feature = "opencv-types")]
            Value::Vector4F(_) => TypeId::of::<opencv::core::Mat4f>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Mat44F(_) => TypeId::of::<opencv::core::Matx44f>(),
            #[cfg(feature = "opencv-types")]
            Value::Mat33F(_) => TypeId::of::<opencv::core::Matx33f>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Mat(_) => TypeId::of::<opencv::core::Mat>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Rect2I(_) => TypeId::of::<opencv::core::Rect2i>(),
            #[cfg(feature = "opencv-types")]
            Value::Rect2F(_) => TypeId::of::<opencv::core::Rect2f>(),
    
            #[cfg(feature = "opencv-types")]
            Value::Size2I(_) => TypeId::of::<opencv::core::Size2i>(),
            #[cfg(feature = "opencv-types")]
            Value::Size2F(_) => TypeId::of::<opencv::core::Size2f>(),
    
            Value::Variant(b) => (*b).type_id(),

            Value::Custom(b) => (*b).type_id(),
        }
    }
}


type ConversionFn = Box<dyn Fn(&Value) -> Option<Value> + Send + Sync>;

static CONVERSIONS: Lazy<RwLock<HashMap<(TypeId, TypeId), ConversionFn>>> = Lazy::new(|| {
    RwLock::new(HashMap::new())
});

pub trait ValueType: Clone + Send + Sync + 'static {
    fn type_id(&self) -> TypeId;
    // Consumes the value
    fn into_value(self) -> Value;
    // Clones the value
    fn to_value(&self) -> Value;
    // Consumes on success
    fn take_value(value: &Value) -> Option<Self> where Self: Sized;
    // Clones the value
    fn from_value(value: &Value) -> Option<Self> where Self: Sized;
}

pub fn register_conversion_fn<F>(from: TypeId, to: TypeId, func: F)
where
    F: Fn(&Value) -> Option<Value> + Send + Sync + 'static,
{
    CONVERSIONS
        .write()
        .insert((from, to), Box::new(func));
}

pub fn convert(value: &Value, to: TypeId) -> Option<Value> {
    CONVERSIONS.read()
        .get(&(value.inner_type_id(), to))
        .and_then(|f| f(value))
}

#[derive(Variation, Clone)]
pub enum Variant {
    Single(Value),
    Vector(Vec<Value>),
}

impl From<Variant> for Value {
    fn from(value: Variant) -> Self {
        Value::Variant(Box::new(value))
    }
}

impl From<Value> for Variant {
    fn from(value: Value) -> Self {
        Variant::Single(value)
    }
}

impl From<Vec<Value>> for Variant {
    fn from(value: Vec<Value>) -> Self {
        Variant::Vector(value)
    }
}

impl<T:ValueType> From<Vec<T>> for Variant {
    fn from(value: Vec<T>) -> Self {
        Variant::Vector(value.into_iter().map(|v| v.into_value()).collect())
    }
}

impl TryFrom<Variant> for Vec<Value> {
    type Error = anyhow::Error;
    fn try_from(value: Variant) -> Result<Self, Self::Error> {
        if let Variant::Vector(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("Not a vector"))
        }
    }
}

impl<T:ValueType> TryFrom<Variant> for Vec<T> {
    type Error = anyhow::Error;
    fn try_from(value: Variant) -> Result<Self, Self::Error> {
        if let Variant::Vector(v) = value {
            if !v.iter().all(|i| i.inner_type_id() == TypeId::of::<T>()) { Err(anyhow!("Not all of same inner type")) }
            else {
                Ok(v.into_iter().map(|i| T::take_value(&i).unwrap()).collect())
            }
        } else {
            Err(anyhow!("Not a vector"))
        }
    }
}

impl<T:ValueType> TryFrom<&Variant> for Vec<T> {
    type Error = anyhow::Error;
    fn try_from(value: &Variant) -> Result<Self, Self::Error> {
        if let Variant::Vector(v) = value {
            if !v.iter().all(|i| i.inner_type_id() == TypeId::of::<T>()) { Err(anyhow!("Not all of same inner type")) }
            else {
                Ok(v.iter().map(|i| T::from_value(&i).unwrap()).collect())
            }
        } else {
            Err(anyhow!("Not a vector"))
        }
    }
}

// impl<T:std::convert::TryFrom<Value> + 'static> TryFrom<Variant> for Vec<T> {
//     type Error = anyhow::Error;
//     fn try_from(value: Variant) -> Result<Self, Self::Error> {
//         if let Variant::Vector(v) = value {
//             if !v.iter().all(|i| i.inner_type_id() == TypeId::of::<T>()) { Err(anyhow!("Not all of same inner type")) }
//             else {
//                 let res = v.into_iter().map(|i| i.try_into());
//                 if res.all(|i| i.is_ok()) {
//                     Ok(res.map(|i| i.ok().unwrap()).collect())
//                 } else {
//                     Err(anyhow!("Conversion errors"))
//                 }
//             }
//         } else {
//             Err(anyhow!("Not a vector"))
//         }
//     }
// }

#[macro_export]
macro_rules! impl_value_conversion {
    ($t:ty, $variant:ident) => {
        impl TryFrom<&Value> for $t {
            type Error = anyhow::Error;
            fn try_from(v: &Value) -> Result<Self, Self::Error> {
                match <$t>::from_value(v) {
                    Some(r) => Ok(r),
                    None => Err(anyhow!("Type mismatch")),
                }
            }
        }

        impl TryFrom<Value> for $t {
            type Error = anyhow::Error;
            fn try_from(v: Value) -> Result<Self, Self::Error> {
                match <$t>::take_value(&v) {
                    Some(r) => Ok(r),
                    None => Err(anyhow!("Type mismatch")),
                }
            }
        }

        impl From<$t> for Value {
            fn from(v: $t) -> Self {
                v.into_value()
            }
        }
    };

    // For custom boxed types
    (custom $t:ty) => {
        impl TryFrom<&Value> for &$t {
            type Error = anyhow::Error;
            fn try_from(v: &Value) -> Result<Self, Self::Error> {
                if let Value::Custom(b) = v {
                    b.downcast_ref::<$t>().ok_or(anyhow!("Type mismatch"))
                } else {
                    Err(anyhow!("Type mismatch"))
                }
            }
        }

        impl TryFrom<Value> for $t {
            type Error = anyhow::Error;
            fn try_from(v: Value) -> Result<Self, Self::Error> {
                if let Value::Custom(b) = v {
                    b.downcast_ref::<$t>().ok_or(anyhow!("Type mismatch"))
                } else {
                    Err(anyhow!("Type mismatch"))
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
            fn into_value(self) -> Value { Value::$variant(self) }
            fn to_value(&self) -> Value { Value::$variant(self.clone()) }
            fn take_value(value: &Value) -> Option<Self> {
                if let Value::$variant(v) = value {
                    Some(v.to_owned())
                } else {
                    None
                }
            }
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
    // Dimension Condition
    (dim $self:ident, $($dims:literal),+) => (register_mat!(dim_mid $self, 0, $($dims),+));
    (dim_mid $self:ident, $index:expr, $dims:literal) => ($self.mat_size().dims() > $index && $self.mat_size().get($index).unwrap() == $dims);
    (dim_mid $self:ident, $index:expr, $dims:literal, $($dims2:literal),+) => {
        register_mat!(dim_mid $self, $index, $dims) && register_mat!(dim_mid $self, $index + 1, $($dims2),+)
    };

    // type_id implementation
    (typ_id $({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        fn type_id(&self) -> TypeId {
            register_mat!(typ_id_mid self, $({$t, $variant, $cv_t, $($dims),+}),+);
            TypeId::of::<opencv::core::Mat>()
        }
    };
    (typ_id_impl $self:ident, $t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+) => {
        if $self.typ() == $cv_t && register_mat!(dim $self, $($dims),+) {
            return TypeId::of::<$t>();
        }
    };
    (typ_id_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        register_mat!(typ_id_impl $self, $t, $variant, $cv_t, $($dims), +);
        register_mat!(typ_id_mid $self, $({$t2, $variant2, $cv_t2, $($dims2), +}), +)
    };
    (typ_id_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        register_mat!(typ_id_impl $self, $t, $variant, $cv_t, $($dims), +)
    };

    // to_value implementation
    (to_val $({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        fn to_value(&self) -> Value {
            register_mat!(to_val_mid self, $({$t, $variant, $cv_t, $($dims),+}),+);
            Value::Mat(self.clone())
        }
    };
    (to_val_impl $self:ident, $t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+) => {
        if $self.typ() == $cv_t && register_mat!(dim $self, $($dims),+) {
            return Value::$variant($self.clone());
        }
    };
    (to_val_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        register_mat!(to_val_impl $self, $t, $variant, $cv_t, $($dims), +);
        register_mat!(to_val_mid $self, $({$t2, $variant2, $cv_t2, $($dims2), +}), +);
    };
    (to_val_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        register_mat!(to_val_impl $self, $t, $variant, $cv_t, $($dims), +);
    };
    
    // into_value implementation
    (into_val $({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        fn into_value(self) -> Value {
            register_mat!(into_val_mid self, $({$t, $variant, $cv_t, $($dims),+}),+);
            Value::Mat(self)
        }
    };
    (into_val_impl $self:ident, $t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+) => {
        if $self.typ() == $cv_t && register_mat!(dim $self, $($dims),+) {
            return Value::$variant($self);
        }
    };
    (into_val_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        register_mat!(into_val_impl $self, $t, $variant, $cv_t, $($dims), +);
        register_mat!(into_val_mid $self, $({$t2, $variant2, $cv_t2, $($dims2), +}), +);
    };
    (into_val_mid $self:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        register_mat!(into_val_impl $self, $t, $variant, $cv_t, $($dims), +);
    };

    // from_value implentation
    (from_val $({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        fn from_value(value: &Value) -> Option<Self> {
            register_mat!(from_val_mid value, $({$t, $variant, $cv_t, $($dims),+}),+);
            if let Value::Mat(v) = value {
                return Some(v.clone());
            }
            None
        }
    };
    (from_val_impl $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        if let Value::$variant(v) = $value {
            return Some(v.clone());
        }
    };
    (from_val_mid $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        register_mat!(from_val_impl $value, {$t, $variant, $cv_t, $($dims), +});
        register_mat!(from_val_mid $value, $({$t2, $variant2, $cv_t2, $($dims2), +}), +);
    };
    (from_val_mid $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        register_mat!(from_val_impl $value, {$t, $variant, $cv_t, $($dims), +});
    };

    // take_value implentation
    (take_val $({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        fn take_value(value: &Value) -> Option<Self> {
            register_mat!(take_val_mid value, $({$t, $variant, $cv_t, $($dims),+}),+);
            if let Value::Mat(v) = value {
                return Some(v.to_owned());
            }
            None
        }
    };
    (take_val_impl $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        if let Value::$variant(v) = $value {
            return Some(v.to_owned());
        }
    };
    (take_val_mid $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}, $({$t2:ty, $variant2:ident, $cv_t2:expr, $($dims2:literal),+}),+) => {
        register_mat!(take_val_impl $value, {$t, $variant, $cv_t, $($dims), +});
        register_mat!(take_val_mid $value, $({$t2, $variant2, $cv_t2, $($dims2), +}), +);
    };
    (take_val_mid $value:ident, {$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}) => {
        register_mat!(take_val_impl $value, {$t, $variant, $cv_t, $($dims), +});
    };

    ($({$t:ty, $variant:ident, $cv_t:expr, $($dims:literal),+}),+) => {
        impl ValueType for opencv::core::Mat {
            register_mat!(typ_id $({$t, $variant, $cv_t, $($dims), +}), +);
            register_mat!(into_val $({$t, $variant, $cv_t, $($dims), +}), +);
            register_mat!(to_val $({$t, $variant, $cv_t, $($dims), +}), +);
            register_mat!(take_val $({$t, $variant, $cv_t, $($dims), +}), +);
            register_mat!(from_val $({$t, $variant, $cv_t, $($dims), +}), +);
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
        $crate::variant::register_conversion_fn(
            TypeId::of::<$from>(),
            TypeId::of::<$to>(),
            |val| {
                if let Some(v) = <$from as $crate::variant::ValueType>::take_value(val) {
                    let out: $to = $func(v);
                    Some(out.to_value())
                } else {
                    None
                }
            }
        );
    }};
}

impl ValueType for String {
    fn type_id(&self) -> TypeId {
        TypeId::of::<String>()
    }
    fn into_value(self) -> Value {
        Value::String(self)
    }
    fn to_value(&self) -> Value {
        Value::String(self.clone())
    }
    fn take_value(value: &Value) -> Option<Self>{
        if let Value::String(v) = value {
            Some(v.to_owned())
        }else {
            None
        }
    }
    fn from_value(value: &Value) -> Option<Self>{
        if let Value::String(v) = value {
            Some(v.clone())
        }else {
            None
        }
    }
}

register_value_type!(bool, Boolean);
register_value_type!(u16, Word);
register_value_type!(i32, Int);
register_value_type!(f32, Float);
register_value_type!(Uuid, UUID);

#[cfg(feature = "opencv-types")]
register_mat!(
    { opencv::core::Mat1b, Vector1B, opencv::core::CV_8U, 1 },
    { opencv::core::Mat2b, Vector2B, opencv::core::CV_8U, 2 },
    { opencv::core::Mat3b, Vector3B, opencv::core::CV_8U, 3 },
    { opencv::core::Mat4b, Vector4B, opencv::core::CV_8U, 4 },
    { opencv::core::Mat1w, Vector1W, opencv::core::CV_16U, 1 },
    { opencv::core::Mat2w, Vector2W, opencv::core::CV_16U, 2 },
    { opencv::core::Mat3w, Vector3W, opencv::core::CV_16U, 3 },
    { opencv::core::Mat4w, Vector4W, opencv::core::CV_16U, 4 },
    { opencv::core::Mat1i, Vector1I, opencv::core::CV_32S, 1 },
    { opencv::core::Mat2i, Vector2I, opencv::core::CV_32S, 2 },
    { opencv::core::Mat3i, Vector3I, opencv::core::CV_32S, 3 },
    { opencv::core::Mat4i, Vector4I, opencv::core::CV_32S, 4 },
    { opencv::core::Mat1f, Vector1F, opencv::core::CV_32F, 1 },
    { opencv::core::Mat2f, Vector2F, opencv::core::CV_32F, 2 },
    { opencv::core::Mat3f, Vector3F, opencv::core::CV_32F, 3 },
    { opencv::core::Mat4f, Vector4F, opencv::core::CV_32F, 4 }
    // { mat opencv::core::Matx33f, Mat33F, opencv::core::CV_32F, 3, 3 },
    // { mat opencv::core::Matx44f, Mat44F, opencv::core::CV_32F, 4, 4 }
);

// register_mat!(
//     // { opencv::core::Mat1b, Vector1B, opencv::core::CV_8U, 1 },
//     // { opencv::core::Mat2b, Vector2B, opencv::core::CV_8U, 2 },
//     // { opencv::core::Mat3b, Vector3B, opencv::core::CV_8U, 3 },
//     // { opencv::core::Mat4b, Vector4B, opencv::core::CV_8U, 4 },
//     // { opencv::core::Mat1i, Vector1I, opencv::core::CV_32S, 1 },
//     // { opencv::core::Mat2i, Vector2I, opencv::core::CV_32S, 2 },
//     // { opencv::core::Mat3i, Vector3I, opencv::core::CV_32S, 3 },
//     // { opencv::core::Mat4i, Vector4I, opencv::core::CV_32S, 4 },
//     // { opencv::core::Mat1f, Vector1F, opencv::core::CV_32F, 1 },
//     // { opencv::core::Mat2f, Vector2F, opencv::core::CV_32F, 2 },
//     // { opencv::core::Mat3f, Vector3F, opencv::core::CV_32F, 3 },
//     { opencv::core::Mat4f, Vector4F, opencv::core::CV_32F, 4 }
// );
#[cfg(feature = "opencv-types")]
register_value_type!(opencv::core::Rect2i, Rect2I);
#[cfg(feature = "opencv-types")]
register_value_type!(opencv::core::Rect2f, Rect2F);
#[cfg(feature = "opencv-types")]
register_value_type!(opencv::core::Size2i, Size2I);
#[cfg(feature = "opencv-types")]
register_value_type!(opencv::core::Size2f, Size2F);
// register_value_type!(opencv::core::Mat, Mat);

impl_value_conversion!(bool, Boolean);
impl_value_conversion!(u16, Word);
impl_value_conversion!(i32, Int);
impl_value_conversion!(f32, Float);
impl_value_conversion!(Uuid, UUID);
#[cfg(feature = "opencv-types")]
impl_value_conversion!(opencv::core::Mat, Mat);

pub fn base_conversions() {
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
    // register_conversion!(opencv::core::Mat => opencv::core::Mat,
    //     |v: opencv::core::Mat | v.clone());

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

#[cfg(test)]
mod tests {
    use std::any::TypeId;
    use crate::register_conversion;
    use super::*;
    use paste::paste;

    #[cfg(feature = "opencv-types")]
    use opencv::core::{self as cv};

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
            #[cfg(feature = "opencv-types")]
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

}
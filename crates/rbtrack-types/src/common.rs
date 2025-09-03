pub mod sync {
    pub use std::sync::{
        Arc,
        Weak,
        LazyLock,
        Barrier,
        BarrierWaitResult,
    };
    pub use parking_lot::*;
}


use crate::define_node;
use crate::node::PopulateNode;
use super::Node;
use super::ports::{Input,Output};
use rbtrack_types::Variant;
use rbtrack_types::sync::{Arc,RwLock};

#[cfg(test)]

pub struct MyNode {
    id:uuid::Uuid,
    label:String,
    desc:String,
    parent_id:Option<uuid::Uuid>,
    in1:Input,
    in2:Input,
    out1:Output,
    inputs:Vec<Arc<RwLock<Input>>>,
    outputs:Vec<Arc<RwLock<Output>>>,
    accum:i32,
    pub active:bool,
}

impl MyNode {
    
}

#[test]
fn use_node() {
}


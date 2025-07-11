use crate::node::ProcessingNode;
use crate::Port;

use super::{ItemInfo,ItemID,ItemDesc,ItemLabel,ItemIndex,Item};

use super::{Node};
use super::ports::{Input,Output};
use rbtrack_types::{Variant,Value};
use rbtrack_types::sync::{Arc,RwLock};


pub struct MyNode {
    info:ItemInfo,
    scalar_in:Arc<RwLock<Input>>,
    unit_in:Arc<RwLock<Input>>,
    result_out:Arc<RwLock<Output>>,
    inputs:Vec<Arc<RwLock<Input>>>,
    outputs:Vec<Arc<RwLock<Output>>>,
    accum:i32,
    pub active:bool,
}

impl MyNode {
    pub fn new(label:ItemLabel, desc:ItemDesc, parent_id:Option<ItemID>) -> Self {
        let id = ItemID::new_v4();
        let scalar_in = Arc::new(RwLock::new(Input::new("scalar".to_string(), Value::from(0.0).into(), Some("Value".to_string()), Some(id))));
        let unit_in = Arc::new(RwLock::new(Input::new("unit".to_string(), Value::String("m".to_string()).into(), Some("Unit".to_string()), Some(id))));
        let result_out = Arc::new(RwLock::new(Output::new("result".to_string(), Value::String("".to_string()).into(), Some("Result".to_string()), Some(id))));
        Self {
            info:ItemInfo { id: id, label: label, desc: desc, parent_id: parent_id },
            scalar_in:scalar_in.clone(),
            unit_in:unit_in.clone(),
            result_out:result_out.clone(),
            inputs:vec![scalar_in,unit_in],
            outputs:vec![result_out],
            accum:0,
            active:false,
        }
    }
}

impl Item for MyNode {
    fn info(&self) -> &ItemInfo {
        &self.info
    }
    fn info_mut(&mut self) -> &mut ItemInfo {
        &mut self.info
    }
}

impl ProcessingNode for MyNode {
    fn compile(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        Ok(())
    }
    fn run(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        *self.result_out.write().get_value().write() = Variant::Single(Value::String(
            std::format!("{} {}",
            self.scalar_in.read().get_value().read().as_single().unwrap().as_float().unwrap(),
            self.unit_in.read().get_value().read().as_single().unwrap().as_string().unwrap())
        ));
        Ok(())
    }
    fn inputs(&self) -> &Vec<Arc<RwLock<Input>>> {
        &self.inputs
    }
    fn inputs_mut(&mut self) -> &mut Vec<Arc<RwLock<Input>>> {
        &mut self.inputs
    }
    fn outputs(&self) -> &Vec<Arc<RwLock<Output>>> {
        &self.outputs
    }
    fn outputs_mut(&mut self) -> &mut Vec<Arc<RwLock<Output>>> {
        &mut self.outputs
    }
}

impl Node for MyNode {
}

#[cfg(test)]
#[test]
fn use_node() {
    let mut node = MyNode::new("MyNode".to_string(), "A test node".to_string(), None);
    assert_eq!(*node.scalar_in.read().get_value().read().as_single().unwrap().as_float().unwrap(), 0.0);
    assert_eq!(*node.unit_in.read().get_value().read().as_single().unwrap().as_string().unwrap(), "m");

    *node.scalar_in.read().get_value().write() = Variant::Single(10.5.into());
    *node.unit_in.read().get_value().write() = Variant::Single(Value::String("m/s".to_string()));

    let _ = node.run();

    assert_eq!(*node.result_out.read().get_value().read().as_single().unwrap().as_string().unwrap(), "10.5 m/s");
}


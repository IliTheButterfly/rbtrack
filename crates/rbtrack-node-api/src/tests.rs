use std::any::TypeId;

use crate::node::ProcessingNode;
use crate::Port;

use super::{ItemInfo,ItemID,ItemDesc,ItemLabel,ItemIndex,Item};

use super::{Node};
use super::ports::{Input,Output};
use anyhow::anyhow;
use rbtrack_types::errors::BTrackError;
use rbtrack_types::{Value, ValueType, Variant,variant::base_conversions};
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

pub struct IntSumNode {
    info:ItemInfo,
    a_in:Arc<RwLock<Input>>,
    b_in:Arc<RwLock<Input>>,
    result_out:Arc<RwLock<Output>>,
    inputs:Vec<Arc<RwLock<Input>>>,
    outputs:Vec<Arc<RwLock<Output>>>,
}

impl IntSumNode {
    pub fn new(parent_id:Option<ItemID>) -> Self {
        let id = ItemID::new_v4();
        let a_in = Arc::new(RwLock::new(Input::new("a".to_string(), Variant::Single(0.into()), None, Some(id))));
        let b_in = Arc::new(RwLock::new(Input::new("b".to_string(), Variant::Single(0.into()), None, Some(id))));
        let result_out = Arc::new(RwLock::new(Output::new("result".to_string(), Variant::Single(0.into()), None, Some(id))));
        Self {
            info:ItemInfo { id:id, label: "int_sum".to_string(), desc: "".to_string(), parent_id: parent_id },
            a_in:a_in.clone(),
            b_in:b_in.clone(),
            result_out:result_out.clone(),
            inputs:vec![a_in, b_in],
            outputs:vec![result_out],
        }
    }
}

impl Item for IntSumNode {
    fn info(&self) -> &ItemInfo {
        &self.info
    }
    fn info_mut(&mut self) -> &mut ItemInfo {
        &mut self.info
    }
}

impl ProcessingNode for IntSumNode {
    fn compile(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        Ok(())
    }
    fn run(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        *self.result_out.write_arc().write() = Variant::Single((
            self.a_in.read_arc()
                .read()
                .as_single()
                .ok_or(BTrackError::Other(anyhow!("Failed to unwrap")))?
                .as_int()
                .ok_or(BTrackError::Other(anyhow!("Failed to unwrap")))? + 
            self.b_in.read_arc()
                .read()
                .as_single()
                .ok_or(BTrackError::Other(anyhow!("Failed to unwrap")))?
                .as_int()
                .ok_or(BTrackError::Other(anyhow!("Failed to unwrap")))?
        ).into());
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

impl Node for IntSumNode {}

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

#[test]
fn chain_nodes() {
    base_conversions();
    
    let mut sum1 = IntSumNode::new(None);
    let mut sum2 = IntSumNode::new(None);
    let mut sum3 = IntSumNode::new(None);

    assert!(sum2.a_in.write_arc().connect(&sum1.result_out).is_ok());
    assert!(sum3.b_in.write_arc().connect(&sum2.result_out).is_ok());

    /*
    s1 = 10 + 15 = 25
    s2 = s1 + 2 = 25 + 2 = 27
    s3 = 2 + s2 = 2 + 27 = 29
    */
    *sum1.a_in.read_arc().write().as_single_mut().unwrap() = 10.into();
    *sum1.b_in.read_arc().write().as_single_mut().unwrap() = 15.into();
    *sum2.b_in.read_arc().write().as_single_mut().unwrap() = 2.into();
    *sum3.a_in.read_arc().write().as_single_mut().unwrap() = 2.into();


    assert!(sum1.run().is_ok());
    assert!(sum2.run().is_ok());
    assert!(sum3.run().is_ok());
    assert_eq!(*sum1.result_out.read_arc().read().as_single().unwrap().as_int().unwrap(), 25);
    assert_eq!(*sum2.result_out.read_arc().read().as_single().unwrap().as_int().unwrap(), 27);
    assert_eq!(*sum3.result_out.read_arc().read().as_single().unwrap().as_int().unwrap(), 29);
}


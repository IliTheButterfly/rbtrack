
use super::{Item,ItemDesc,ItemID,ItemIndex,ItemInfo,ItemLabel};
use super::ports::{Input,Output};
use rbtrack_types::sync::{Arc,RwLock};
use rbtrack_types::errors::*;
use anyhow::{anyhow, Result};

pub trait ProcessingNode: Send + Sync + Item {
    fn inputs(&self) -> &Vec<Arc<RwLock<Input>>>;
    fn inputs_mut(&mut self) -> &mut Vec<Arc<RwLock<Input>>>;
    fn outputs(&self) -> &Vec<Arc<RwLock<Output>>>;
    fn outputs_mut(&mut self) -> &mut Vec<Arc<RwLock<Output>>>;
    fn compile(&mut self) -> Result<(), BTrackError>;
    fn run(&mut self) -> Result<(), BTrackError>;
}

pub trait Node: ProcessingNode {
    fn input_by_index(&self, index:&ItemIndex) -> Option<Arc<RwLock<Input>>> {
        match self.inputs().get(*index) {
            Some(input) => Some(input.clone()),
            None => None,
        }
    }
    fn input_by_id(&self, id:&ItemID) -> Option<Arc<RwLock<Input>>> {
        for input in self.inputs() {
            if input.read().info().id == *id {
                return Some(input.clone())
            }
        }
        None
    }
    fn input_by_label(&self, label:&ItemLabel) -> Option<Arc<RwLock<Input>>> {
        for input in self.inputs() {
            if input.read().info().label == *label {
                return Some(input.clone())
            }
        }
        None
    }
    fn output_by_index(&self, index:&ItemIndex) -> Option<Arc<RwLock<Output>>> {
        match self.outputs().get(*index) {
            Some(output) => Some(output.clone()),
            None => None,
        }
    }
    fn output_by_id(&self, id:&ItemID) -> Option<Arc<RwLock<Output>>> {
        for output in self.outputs() {
            if output.read().info().id == *id {
                return Some(output.clone())
            }
        }
        None
    }
    fn output_by_label(&self, label:&ItemLabel) -> Option<Arc<RwLock<Output>>> {
        for output in self.outputs() {
            if output.read().info().label == *label {
                return Some(output.clone())
            }
        }
        None
    }
}

use super::Item;
use super::ports::{Input,Output};
use rbtrack_types::sync::{Arc,Weak,RwLock};
use std::collections::HashMap;
use rbtrack_types::errors::*;
use anyhow::{anyhow, Result};

pub trait ProcessingNode: Send + Sync {
    fn inputs(&self) -> Vec<Arc<RwLock<Input>>>;
    fn outputs(&self) -> Vec<Arc<RwLock<Output>>>;
    fn compile(&mut self) -> Result<(), BTrackError>;
    fn run(&mut self) -> Result<(), BTrackError>;
}

pub trait Node: Item + ProcessingNode {
    fn input_by_index(&self, index:&usize) -> Option<Arc<RwLock<Input>>>;
    fn input_by_id(&self, id:&uuid::Uuid) -> Option<Arc<RwLock<Input>>>;
    fn input_by_name(&self, name:&str) -> Option<Arc<RwLock<Input>>>;
    fn output_by_index(&self, index:&usize) -> Option<Arc<RwLock<Output>>>;
    fn output_by_id(&self, id:&uuid::Uuid) -> Option<Arc<RwLock<Output>>>;
    fn output_by_name(&self, name:&str) -> Option<Arc<RwLock<Output>>>;
}

#[macro_export]
macro_rules! define_node {
    (
        $name:ident {
            $(
                $port_field:ident : $port_ty:ty = {$value_ty:ty, $port_name:literal, $port_desc:literal, $port_default:literal}
            ),* $(,)?
        }
    ) => {
        pub struct $name {
            id:uuid::Uuid,
            label:String,
            desc:String,
            parent_id:Option<uuid::Uuid>,
            $(
                $port_field: $port_ty,
            )*
            inputs: Vec<$crate::>,
            name_map: std::collections::HashMap<&'static str, usize>,
            id_map: std::collections::HashMap<uuid::Uuid, usize>,
        }

        impl $name {
            pub fn new(label:String, desc:String, parent_id:Option<uuid::Uuid>) -> Self {
                let mut ports_vec = vec![];
                let mut name_map = std::collections::HashMap::new();
                let mut id_map = std::collections::HashMap::new();
                let id = uuid::Uuid::new_v4();

                let mut res = Self {
                    id:id.clone(),
                    label,
                    desc,
                    parent_id,
                    $(
                        $port_field: <$port_ty>::new($port_name, rbtrack_types::Variant::Single($port_default.into()), Some($port_desc), Some(id.clone())),
                    )*
                    ports: ports_vec,
                    name_map,
                    id_map,
                };
                $(
                    ports_vec.push(&mut res.$port_field);
                    name_map.insert($port_name, ports_vec.len() - 1);
                    id_map.insert(res.$port_field.id().clone(), ports_vec.len() - 1);
                )*
                res
            }

            pub fn port_by_name(&self, name: &str) -> Option<&$crate::ports::Port> {
                self.name_map.get(name).map(|&i| unsafe { &*self.ports[i] })
            }

            pub fn port_by_id(&self, id: uuid::Uuid) -> Option<&$crate::ports::Port> {
                self.id_map.get(&id).map(|&i| unsafe { &*self.ports[i] })
            }

            pub fn port_by_index(&self, index: usize) -> Option<&$crate::ports::Port> {
                self.ports.get(index).map(|&p| unsafe { &*p })
            }

            $(
                pub fn $port_field(&self) -> &$crate::ports::Port {
                    &self.$port_field
                }

                paste::paste! {
                    pub fn [<$port_field _mut>](&mut self) -> &mut $crate::ports::Port {
                        &mut self.$port_field
                    }
                }
            )*
        }
    };
}

pub struct BaseNode<T:Node> {
    id:uuid::Uuid,
    label:String,
    desc:String,
    parent_id:Option<uuid::Uuid>,
    inputs_name_map: std::collections::HashMap<&'static str, usize>,
    inputs_id_map: std::collections::HashMap<uuid::Uuid, usize>,
    outputs_name_map: std::collections::HashMap<&'static str, usize>,
    outputs_id_map: std::collections::HashMap<uuid::Uuid, usize>,
    subnode:Box<T>,
}

impl<T:ProcessingNode> BaseNode<T> {
    pub fn new(label:&str, desc:&str, parent_id:&Option<uuid::Uuid>, subnode:T) -> Self {
        Self {
            id:uuid::Uuid::new_v4(),
            label:label.to_string(),
            desc:desc.to_string(),
            parent_id:parent_id.clone(),
            inputs_name_map:HashMap::<&'static str, usize>::new(),
            inputs_id_map:HashMap::<uuid::Uuid, usize>::new(),
            outputs_name_map:HashMap::<&'static str, usize>::new(),
            outputs_id_map:HashMap::<uuid::Uuid, usize>::new(),
            subnode:subnode,
        }
    }
}

impl<T:Send + Sync> Item for BaseNode<T> {
    fn id(&self) -> &uuid::Uuid {
        &self.id
    }
    fn id_mut(&mut self) -> &mut uuid::Uuid {
        &mut self.id
    }
    fn label(&self) -> &String {
        &self.label
    }
    fn label_mut(&mut self) -> &mut String {
        &mut self.label
    }
    fn desc(&self) -> &String {
        &self.desc
    }
    fn desc_mut(&mut self) -> &mut String {
        &mut self.desc
    }
    fn parent_id(&self) -> &Option<uuid::Uuid> {
        &self.parent_id
    }
    fn parent_id_mut(&mut self) -> &mut Option<uuid::Uuid> {
        &mut self.parent_id
    }
}

impl<T:Send + Sync> Node for BaseNode<T> {
    fn compile(&mut self) -> Result<(), BTrackError> {
        if let Some(func) = &self.compile_fn {
            return func.read()(&mut self.context)
        }
        Err(BTrackError::Other(anyhow!("compile_fn is None")))
    }
    fn run(&mut self) -> Result<(), BTrackError> {
        if let Some(func) = &self.run_fn {
            return func.read()(&mut self.context)
        }
        Err(BTrackError::Other(anyhow!("run_fn is None")))
    }
    fn input_by_index(&self, index:&usize) -> Option<Arc<RwLock<Input>>> {
        if let Some(input) = self.inputs.get(*index) {
            return Some(input.clone());
        }
        None
    }
    fn input_by_id(&self, id:&uuid::Uuid) -> Option<Arc<RwLock<Input>>> {
        if let Some(index) = self.inputs_id_map.get(id) {
            return self.input_by_index(index);
        }
        None
    }
    fn input_by_name(&self, name:&str) -> Option<Arc<RwLock<Input>>> {
        if let Some(index) = self.inputs_name_map.get(name) {
            return self.input_by_index(index);
        }
        None
    }
    fn output_by_index(&self, index:&usize) -> Option<Arc<RwLock<Output>>> {
        if let Some(output) = self.outputs.get(*index) {
            return Some(output.clone());
        }
        None
    }
    fn output_by_id(&self, id:&uuid::Uuid) -> Option<Arc<RwLock<Output>>> {
        if let Some(index) = self.outputs_id_map.get(id) {
            return self.output_by_index(index);
        }
        None
    }
    fn output_by_name(&self, name:&str) -> Option<Arc<RwLock<Output>>> {
        if let Some(index) = self.outputs_name_map.get(name) {
            return self.output_by_index(index);
        }
        None
    }
}

impl<T:Send + Sync + Clone> Clone for BaseNode<T> {
    fn clone(&self) -> Self {
        Self {
            id:uuid::Uuid::new_v4(),
            label:self.label.clone(),
            desc:self.desc.clone(),
            parent_id:self.parent_id.clone(),

        }
    }
}
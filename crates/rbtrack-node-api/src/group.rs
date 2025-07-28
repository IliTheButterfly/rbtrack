use super::{Item,ItemDesc,ItemID,ItemIndex,ItemInfo,ItemLabel,Node,Port,ProcessingNode,ports::{Input,Output}};
use rbtrack_types::{Value,Variant};
use rbtrack_types::sync::{Arc,RwLock,Weak};
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::algo::toposort;
use std::collections::HashMap;

pub trait GraphSolver: Sync + Send {
    fn solve(&self, nodes:&Vec<Arc<RwLock<dyn Node>>>) -> Vec<Arc<RwLock<dyn Node>>>;
}

pub struct TopologicalSolver {
}

impl GraphSolver for TopologicalSolver {
    fn solve(&self, nodes:&Vec<Arc<RwLock<dyn Node>>>) -> Vec<Arc<RwLock<dyn Node>>> {
        // Reserve variables
        let mut graph = DiGraph::<ItemID, ()>::new();
        let mut result = Vec::<Arc<RwLock<dyn Node>>>::with_capacity(nodes.len());
        let mut id_node_map = HashMap::<ItemID, Arc<RwLock<dyn Node>>>::with_capacity(nodes.len());
        let mut index_id_map = HashMap::<NodeIndex, ItemID>::with_capacity(nodes.len());
        let mut id_index_map = HashMap::<ItemID, NodeIndex>::with_capacity(nodes.len());
        graph.reserve_nodes(nodes.len());

        // Compile nodes
        for node in nodes {
            let _ = node.write_arc().compile();
            let id = node.read().info().id.clone();
            let n = graph.add_node(id);
            id_node_map.insert(id, node.clone());
            index_id_map.insert(n, id);
            id_index_map.insert(id, n);
        }

        // Create adjacency list
        for node in nodes {
            let node = node.read_arc();
            for input in node.inputs() {
                if let Some(weak) = input.read_arc().connection() {
                    if let Some(output) = weak.upgrade() {
                        if let Some(parent_id) = &output.read_arc().info().parent_id {
                            graph.update_edge(*id_index_map.get(parent_id).unwrap(), *id_index_map.get(&node.info().id).unwrap(), ());
                        }
                    }
                }
            }
        }

        // Sort the node graph
        match toposort(&graph, None) {
            Ok(sorted) => {
                for n in sorted.iter().rev() {
                    if let Some(id) = index_id_map.get(n) {
                        if let Some(node) = id_node_map.get(id) {
                            result.push(node.clone());
                        }
                    }
                }
            },
            Err(_) => {
            },
        }

        result
    }
}

pub trait Group: Node {
    fn nodes(&self) -> &Vec<Arc<RwLock<dyn Node>>>;
    fn nodes_mut(&mut self) -> &mut Vec<Arc<RwLock<dyn Node>>>;
}

pub struct BaseGroup {
    info:ItemInfo,
    nodes:Vec<Arc<RwLock<dyn Node>>>,
    solver:Box<dyn GraphSolver>,
    order:Vec<Arc<RwLock<dyn Node>>>,
    inputs:Vec<Arc<RwLock<Input>>>,
    outputs:Vec<Arc<RwLock<Output>>>,
}

impl BaseGroup {
    pub fn new(label:ItemLabel, desc:ItemDesc, solver:Box<dyn GraphSolver>, parent_id:Option<ItemID>) -> Self {
        Self {
            info:ItemInfo { id: ItemID::new_v4(), label: label, desc: desc, parent_id: parent_id },
            nodes:vec![],
            solver,
            order:vec![],
            inputs:vec![],
            outputs:vec![],
        }
    }
}

impl Item for BaseGroup {
    fn info(&self) -> &ItemInfo {
        &self.info
    }
    fn info_mut(&mut self) -> &mut ItemInfo {
        &mut self.info
    }
}

impl ProcessingNode for BaseGroup {
    fn compile(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        self.order = self.solver.solve(&self.nodes);
        Ok(())
    }
    fn run(&mut self) -> anyhow::Result<(), rbtrack_types::errors::BTrackError> {
        for node in &self.order {
            if let Err(err) = node.write_arc().run() {
                return Err(err);
            }
        }
        return Ok(())
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



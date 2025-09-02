// indexmap = "1.10"   // preserves insertion order (useful for deterministic behavior)

use indexmap::IndexMap;
use itertools::Itertools;
use rbtrack_types::{Shape, TypeSpec, Value};
use rbtrack_types::sync::{Arc, RwLock, Weak};
use std::fmt::{Debug, Display};
use std::{
    any::TypeId,
    collections::{HashMap, HashSet, VecDeque},
};
use uuid::Uuid;


/// -------------------------
/// IDs
/// -------------------------
/// These help differentiating between templates/instances and item/ports IDs
/// This is very useful because those IDs are incompatible


// ID of a template stored in the registrar
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd, Debug, Hash)]
pub struct TemplateID(Uuid);

// ID of an instance inside a group
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd, Debug, Hash)]
pub struct InstanceID(Uuid);

// ID of a template port within an item template
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd, Debug, Hash)]
pub struct PortTemplateID(Uuid);

// ID of a instance port within an item instance
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd, Debug, Hash)]
pub struct PortInstanceID(Uuid);

// Helper enum for diagnostics
#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd, Debug, Hash)]
pub enum ItemID {
    Template(TemplateID),
    Instance(InstanceID),
    PortTemplate(PortTemplateID),
    PortInstance(PortInstanceID),
}

impl Display for ItemID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Template(id) => write!(f, "Template: {:?}", id.0.to_string().get(..6)),
            Self::Instance(id) => write!(f, "Instance: {:?}", id.0.to_string().get(..6)),
            Self::PortTemplate(id) => write!(f, "TemplatePort: {:?}", id.0.to_string().get(..6)),
            Self::PortInstance(id) => write!(f, "InstancePort: {:?}", id.0.to_string().get(..6)),
        }
    }
}


/// -------------------------
/// Diagnostics
/// -------------------------

// Severity of the diagnostic
#[derive(Clone, Debug, Copy, Eq, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// A diagnostic struct for compilation debugging
#[derive(Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    /// Optionally point to a template/node/port (by id) for precise UI mapping
    pub related_ids: Vec<ItemID>,
}

impl Debug for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ids: String = itertools::Itertools::intersperse(self.related_ids.iter().map(|id| id.to_string()), ", ".to_string()).collect();
        write!(f, "[{}] {} | IDs: {}", 
            self.severity.to_string(), 
            self.message, 
            ids
            )
    }
}

/// -------------------------
/// Templates
/// -------------------------
 
// Direction of port
#[derive(Clone, Debug, PartialEq, Copy)]
pub enum PortDirection {
    Input,
    Output,
}

// Template of a port
#[derive(Clone, Debug)]
pub struct PortTemplate {
    // ID of the node template that has this port
    pub parent_id: TemplateID,
    // ID of this port template
    pub id: PortTemplateID,
    // Name of this port
    pub name: String,
    // Direction of this port
    pub direction: PortDirection,
    // Type specification of this port
    pub ty: TypeSpec,
    /// For input ports: default value used when dangling (optional)
    pub default: Option<Value>,
}

impl PortTemplate {
    pub fn new_input(parent_id: TemplateID, name: &str, ty: TypeSpec, default: Option<Value>) -> Self {
        Self {
            parent_id: parent_id,
            id: PortTemplateID(Uuid::new_v4()),
            name: name.to_string(),
            direction: PortDirection::Input,
            ty,
            default,
        }
    }
    pub fn new_output(parent_id: TemplateID, name: &str, ty: TypeSpec) -> Self {
        Self {
            parent_id: parent_id,
            id: PortTemplateID(Uuid::new_v4()),
            name: name.to_string(),
            direction: PortDirection::Output,
            ty,
            default: None,
        }
    }
}

// Template of an item
#[derive(Debug, Clone)]
pub struct ItemTemplate {
    // ID of this item template
    pub id: TemplateID,
    // Name of this node
    pub name: String,
    // List of ports
    pub ports: IndexMap<PortTemplateID, PortTemplate>,
    // When used as a group, nodes that are contained by this group
    pub nodes: Option<IndexMap<InstanceID, ItemInstance>>,
    // When used as a group, connections within this group
    pub connections: Option<Vec<Connection>>,
}

impl ItemTemplate {
    pub fn new_atomic(name: &str) -> Self {
        Self {
            id: TemplateID(Uuid::new_v4()),
            name: name.to_string(),
            ports: IndexMap::new(),
            nodes: None,
            connections: None,
        }
    }

    pub fn new_group(name: &str) -> Self {
        Self {
            id: TemplateID(Uuid::new_v4()),
            name: name.to_string(),
            ports: IndexMap::new(),
            nodes: Some(IndexMap::new()),
            connections: Some(vec![]),
        }
    }

    pub fn add_input(&mut self, name: &str, ty: TypeSpec, default: Option<Value>) -> PortTemplateID {
        let port = PortTemplate::new_input(self.id, name, ty, default);
        let id = port.id;
        self.ports.insert(id, port);
        id
    }

    pub fn add_output(&mut self, name: &str, ty: TypeSpec) -> PortTemplateID {
        let port = PortTemplate::new_output(self.id, name, ty);
        let id = port.id;
        self.ports.insert(id, port);
        id
    }

    pub fn port_template_from_id(&self, id: &PortTemplateID) -> Option<&PortTemplate> {
        self.ports.get(id)
    }

    pub fn port_template_from_name(&self, name: &str) -> Option<&PortTemplate> {
        self.ports.values().find(|&p| p.name == name)
    }

    pub fn item_instance_from_id(&self, id: &InstanceID) -> Option<&ItemInstance> {
        self.nodes.as_ref()?.get(id)
    }

    pub fn port_instance_from_id(&self, id: &PortInstanceID) -> Option<&PortInstance> {
        self.nodes
            .as_ref()?
            .values()
            .find_map(|instance| instance.ports.get(id))
    }

    pub fn port_instance_from_template_id(&self, item_id: &InstanceID, template_id: &PortTemplateID) -> Option<&PortInstance> {
        self.nodes
            .as_ref()?
            .get(item_id)?
            .find_port_from_template_id(template_id)
    }
}

impl PartialEq for ItemTemplate {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

/// -------------------------
/// Instances
/// -------------------------

// An instance of a port
#[derive(Debug, Clone)]
pub struct PortInstance {
    // Unique ID of this port instance
    pub id: PortInstanceID,
    // ID of the node instance that contains this port
    pub parent_id: InstanceID,
    // ID of the port template of this port
    pub template_id: PortTemplateID,
}

impl PortInstance {
    pub fn new(parent_id: &InstanceID, template_id: &PortTemplateID) -> Self {
        Self {
            id: PortInstanceID(Uuid::new_v4()),
            parent_id: *parent_id,
            template_id: *template_id,
        }
    }
    pub fn from_template(parent_id: &InstanceID, template: &PortTemplate) -> Self {
        Self::new(parent_id, &template.id)
    }
}

// An instance of an item
#[derive(Debug, Clone)]
pub struct ItemInstance {
    // Unique ID of this instance
    pub id: InstanceID,
    // Group template
    pub parent_id: TemplateID,
    // Template of this node
    pub template_id: TemplateID,
    // Display name
    pub label: String,
    // Port instances
    pub ports: IndexMap<PortInstanceID, PortInstance>,
}

impl ItemInstance {
    pub fn new(parent_id: &TemplateID, template_id: &TemplateID, label: &str) -> Self {
        Self {
            id: InstanceID(Uuid::new_v4()),
            parent_id: *parent_id,
            template_id: *template_id,
            label: label.to_string(),
            ports: IndexMap::new()
        }
    }

    pub fn from_template(parent_id: &TemplateID, template: &ItemTemplate, label: Option<&str>) -> Self {
        let mut res = Self::new(parent_id, &template.id, label.unwrap_or(&template.name));
        res.ports.extend(
            template
                .ports
                .values()
                .map(|pt| {
                    let pi = PortInstance::from_template(&res.id, pt);
                    (pi.id, pi)
                }),
        );
        res
    }

    pub fn find_port_from_template_id(&self, template_id: &PortTemplateID) -> Option<&PortInstance> {
        self.ports.values().find(|port| port.template_id == template_id)
    }
}

// A connection between two port instances
#[derive(Debug, Clone, PartialEq)]
pub struct Connection {
    // ID of the source port instance
    pub from_id: PortInstanceID,
    // ID of the destination port instance
    pub to_id: PortInstanceID,
}

/// -------------------------
/// References
/// -------------------------

// Reference to a port instance
pub struct PortRef {
    pub instance_id: PortInstanceID,
    registrar: Weak<RwLock<Registrar>>,
}

impl PortRef {
    pub fn connections(&self) -> Vec<PortRef> {
        if let Some(registrar) = self.registrar.upgrade().and_then(|registrar| registrar.try_read_arc()) {
            registrar.toolset.find_port_instance(self.instance_id)
        }
    }
}

const GROUP_PORT_SENTINEL: Uuid = Uuid::from_u128(0); // sentinel to represent group-external port refs

/// -------------------------
/// Toolset & Registrar
/// -------------------------
#[derive(Clone, Debug)]
pub struct Toolset {
    pub node_templates: IndexMap<TemplateID, ItemTemplate>,
}

impl Toolset {
    pub fn new() -> Self {
        Self {
            node_templates: IndexMap::new(),
        }
    }

    pub fn register_item_template(&mut self, template: ItemTemplate) -> TemplateID {
        let id = template.id;
        self.node_templates.insert(id, template);
        id
    }

    pub fn find_item_template(&self, id: &TemplateID) -> Option<&ItemTemplate> {
        self.node_templates.get(id)
    }

    pub fn find_item_instance(&self, id: &InstanceID) -> Option<&ItemInstance> {
        self.node_templates
            .values()
            .find_map(|template| template.item_instance_from_id(id))
    }

    pub fn find_port_template(&self, id: &PortTemplateID) -> Option<&PortTemplate> {
        self.node_templates
            .values()
            .find_map(|template| template.port_template_from_id(id))
    }

    pub fn find_port_instance(&self, id: &PortInstanceID) -> Option<&PortInstance> {
        self.node_templates
            .values()
            .find_map(|template| {
                template.nodes.as_ref()?.values().find_map(|inst| inst.ports.get(id))
            })
    }
}

/// Registrar: owns the toolset and entry point (root group)
#[derive(Clone, Debug)]
pub struct Registrar {
    pub toolset: Toolset,
    pub entry_group: Option<TemplateID>, // GroupTemplate id
}

impl Registrar {
    pub fn new() -> Self {
        Self {
            toolset: Toolset::new(),
            entry_group: None,
        }
    }

    pub fn set_entry_point(&mut self, group_id: TemplateID) {
        self.entry_group = Some(group_id);
    }

}

/// -------------------------
/// Helpers
/// -------------------------
fn format_type(ty: &TypeSpec) -> String {
    match &ty.shape {
        Shape::Scalar => format!("{:?}", ty.base),
        Shape::Vector(Some(n)) => format!("Vec<{:?}>[{}]", ty.base, n),
        Shape::Vector(None) => format!("Vec<{:?}>[N]", ty.base),
    }
}

#[cfg(test)]
mod tests {
    use std::any::TypeId;

    use rbtrack_types::TypeSpec;
    use uuid::Uuid;

    use crate::ir::{
        ConnectionTemplate, GROUP_PORT_SENTINEL, GroupTemplate, NodeInstanceTemplate, NodeTemplate,
        PortRef, PortTemplate, Registrar,
    };

    #[test]
    fn example_usage() {
        let mut registrar = Registrar::new();

        // Atomic templates
        let int_value_node = {
            // outputs: value: int
            let mut t = NodeTemplate::new_atomic(
                "IntValue",
                Some("emits an integer value"),
                vec![PortTemplate::new_output(
                    "value",
                    TypeSpec::scalar(TypeId::of::<i32>()),
                )],
            );
            registrar.toolset.register_node_template(t)
        };

        let int_sum_node = {
            // inputs: a:int, b:int  outputs: res:int
            let mut t = NodeTemplate::new_atomic(
                "IntSum",
                Some("adds two ints"),
                vec![
                    PortTemplate::new_input("a", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_input("b", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_output("res", TypeSpec::scalar(TypeId::of::<i32>())),
                ],
            );
            registrar.toolset.register_node_template(t)
        };

        let int_sub_node = {
            let mut t = NodeTemplate::new_atomic(
                "IntSub",
                Some("subtracts two ints"),
                vec![
                    PortTemplate::new_input("a", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_input("b", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_output("res", TypeSpec::scalar(TypeId::of::<i32>())),
                ],
            );
            registrar.toolset.register_node_template(t)
        };

        // After registering atomic node templates (int_value_node, int_sum_node, etc.)
        let triple_group_id = {
            let mut group = GroupTemplate::new(
                "TripleSumGroup",
                Some("adds three ints using two sums"),
                vec![
                    PortTemplate::new_input("a", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_input("b", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_input("c", TypeSpec::scalar(TypeId::of::<i32>()), None),
                    PortTemplate::new_output("res", TypeSpec::scalar(TypeId::of::<i32>())),
                ],
            );

            let sum1_inst = NodeInstanceTemplate {
                instance_id: Uuid::new_v4(),
                template_id: int_sum_node,
                name_hint: Some("sum1".into()),
            };
            let sum2_inst = NodeInstanceTemplate {
                instance_id: Uuid::new_v4(),
                template_id: int_sum_node,
                name_hint: Some("sum2".into()),
            };

            group.add_node(sum1_inst.clone());
            group.add_node(sum2_inst.clone());

            // Now use registrar helpers to add connections by names:
            // group.a -> sum1.a
            registrar
                .add_connection_by_names(
                    &mut group,
                    GROUP_PORT_SENTINEL,
                    "a",
                    sum1_inst.instance_id,
                    "a",
                )
                .expect("failed to add connection");

            // group.b -> sum1.b
            registrar
                .add_connection_by_names(
                    &mut group,
                    GROUP_PORT_SENTINEL,
                    "b",
                    sum1_inst.instance_id,
                    "b",
                )
                .expect("failed to add connection");

            // sum1.res -> sum2.a
            registrar
                .add_connection_by_names(
                    &mut group,
                    sum1_inst.instance_id,
                    "res",
                    sum2_inst.instance_id,
                    "a",
                )
                .expect("failed to add connection");

            // group.c -> sum2.b
            registrar
                .add_connection_by_names(
                    &mut group,
                    GROUP_PORT_SENTINEL,
                    "c",
                    sum2_inst.instance_id,
                    "b",
                )
                .expect("failed to add connection");

            // sum2.res -> group.res
            registrar
                .add_connection_by_names(
                    &mut group,
                    sum2_inst.instance_id,
                    "res",
                    GROUP_PORT_SENTINEL,
                    "res",
                )
                .expect("failed to add connection");

            registrar.toolset.register_group_template(group)
        };

        // MainGroup: omitted due to verbosity; in practice you'd build nodes and connections similarly,
        // referencing node templates by id, using NodeInstanceTemplate entries, and pushing ConnectionTemplate entries.

        // Set entry
        registrar.set_entry_point(triple_group_id);

        // Validate
        let diagnostics = registrar.validate();
        for (gid, diags) in diagnostics {
            println!("Diagnostics for group {}:", gid);
            for d in diags {
                println!(
                    "  - [{:?}] {} (related: {:?})",
                    d.severity, d.message, d.related_ids
                );
            }
        }
    }
}

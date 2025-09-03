// indexmap = "1.10"   // preserves insertion order (useful for deterministic behavior)

use indexmap::IndexMap;
use itertools::Itertools;
use parking_lot::lock_api;
use rbtrack_types::{Shape, TypeSpec, Value};
use rbtrack_types::sync::{Arc, RwLock, Weak, ArcRwLockReadGuard, ArcRwLockWriteGuard, RawRwLock};
use std::fmt::{Debug, Display};
use std::marker::PhantomData;
use std::str::Matches;
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

    pub fn add_item(&mut self, item: &ItemTemplate, label: Option<&str>) -> Option<InstanceID> {
        let instance = ItemInstance::from_template(&self.id, item, label);
        let id = instance.id;
        self.nodes.as_mut()?.insert(id, instance.clone());
        Some(id)
    }

    pub fn remove_item_from_id(&mut self, id: &InstanceID) -> Option<bool> {
        Some(self.nodes.as_mut()?.shift_remove(id).is_some())
    }

    pub fn port_template_from_id<'a>(&'a self, id: &PortTemplateID) -> Option<&'a PortTemplate> {
        self.ports.get(id)
    }

    pub fn port_template_from_id_mut<'a>(&'a mut self, id: &PortTemplateID) -> Option<&'a mut PortTemplate> {
        self.ports.get_mut(id)
    }

    pub fn port_template_from_name<'a>(&'a self, name: &str) -> Option<&'a PortTemplate> {
        self.ports.values().find(|&p| p.name == name)
    }

    pub fn port_template_from_name_mut<'a>(&'a mut self, name: &str) -> Option<&'a mut PortTemplate> {
        self.ports.values_mut().find(|p| p.name == name)
    }

    pub fn item_instance_from_id<'a>(&'a self, id: &InstanceID) -> Option<&'a ItemInstance> {
        self.nodes.as_ref()?.get(id)
    }

    pub fn item_instance_from_id_mut<'a>(&'a mut self, id: &InstanceID) -> Option<&'a mut ItemInstance> {
        self.nodes.as_mut()?.get_mut(id)
    }

    pub fn port_instance_from_id<'a>(&'a self, id: &PortInstanceID) -> Option<&'a PortInstance> {
        self.nodes
            .as_ref()?
            .values()
            .find_map(|instance| instance.ports.get(id))
    }

    pub fn port_instance_from_id_mut<'a>(&'a mut self, id: &PortInstanceID) -> Option<&'a mut PortInstance> {
        self.nodes
            .as_mut()?
            .values_mut()
            .find_map(|instance| instance.ports.get_mut(id))
    }

    pub fn port_instance_from_template_id<'a>(&'a self, item_id: &InstanceID, template_id: &PortTemplateID) -> Option<&'a PortInstance> {
        self.nodes
            .as_ref()?
            .get(item_id)?
            .find_port_from_template_id(template_id)
    }

    pub fn port_instance_from_template_id_mut<'a>(&'a mut self, item_id: &InstanceID, template_id: &PortTemplateID) -> Option<&'a mut PortInstance> {
        self.nodes
            .as_mut()?
            .get_mut(item_id)?
            .find_port_from_template_id_mut(template_id)
    }

    pub fn is_group(&self) -> bool {
        self.nodes.is_some()
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

    pub fn find_port_from_id<'a>(&'a self, id: &PortInstanceID) -> Option<&'a PortInstance> {
        self.ports.get(id)
    }

    pub fn find_port_from_id_mut<'a>(&'a mut self, id: &PortInstanceID) -> Option<&'a mut PortInstance> {
        self.ports.get_mut(id)
    }

    pub fn find_port_from_template_id<'a>(&'a self, template_id: &PortTemplateID) -> Option<&'a PortInstance> {
        self.ports.values().find(|port| &port.template_id == template_id)
    }

    pub fn find_port_from_template_id_mut<'a>(&'a mut self, template_id: &PortTemplateID) -> Option<&'a mut PortInstance> {
        self.ports.values_mut().find(|port| &port.template_id == template_id)
    }
}

// A connection between two port instances
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Connection {
    // ID of the source port instance
    pub from_id: PortInstanceID,
    // ID of the destination port instance
    pub to_id: PortInstanceID,
}

impl Connection {
    pub fn new(from_id: &PortInstanceID, to_id: &PortInstanceID) -> Self {
        Self {
            from_id: *from_id,
            to_id: *to_id,
        }
    }

    pub fn from_ports(from: &PortInstance, to: &PortInstance) -> Self {
        Self::new(&from.id, &to.id)
    }

    pub fn contains_id(&self, id: &PortInstanceID) -> bool {
        &self.from_id == id || &self.to_id == id
    }

    pub fn contains_port(&self, port: &PortInstance) -> bool {
        self.contains_id(&port.id)
    }
}

/// -------------------------
/// References
/// -------------------------

// Reference to a port instance
// This reference cannot modify the registrar
#[derive(Debug, Clone)]
pub struct PortInstanceRef {
    pub id: PortInstanceID,
    registrar: Weak<RwLock<Registrar>>,
}

impl PortInstanceRef {
    pub fn new(registrar: Weak<RwLock<Registrar>>, instance_id: &PortInstanceID) -> Self {
        Self {
            id: *instance_id,
            registrar: registrar,
        }
    }

    pub fn get_instance(&self) -> Option<PortInstanceReadGuard> {
        let arc = self.registrar.upgrade()?;
        let guard = arc.read_arc();
        let id = self.id;
        Some(PortInstanceReadGuard { guard, id })
    }
    
}

pub struct PortInstanceReadGuard {
    guard: ArcRwLockReadGuard<RawRwLock, Registrar>,
    id: PortInstanceID,
}

impl std::ops::Deref for PortInstanceReadGuard {
    type Target = PortInstance;
    fn deref(&self) -> &Self::Target {
        self.guard.
    }
}

// Reference to an item instance
// This reference can modify the registrar
#[derive(Debug, Clone)]
pub struct ItemInstanceRef {
    pub id: InstanceID,
    registrar: Weak<RwLock<Registrar>>,
}

impl ItemInstanceRef {

}

// Reference to an item instance
// This reference can modify the registrar
#[derive(Debug, Clone)]
pub struct ItemTemplateRef {
    pub id: TemplateID,
    registrar: Weak<RwLock<Registrar>>,
}

impl ItemTemplateRef {
    pub fn new(registrar: Weak<RwLock<Registrar>>, template_id: &TemplateID) -> Self {
        Self {
            id: *template_id,
            registrar: registrar,
        }
    }

    pub fn get_name(&self) -> Option<String> {
        let ptr = self.registrar.upgrade()?;
        let registrar = ptr.read();
        let item = registrar.toolset.find_item_template(&self.id)?;
        Some(item.name.clone())
    }

    pub fn get_connections(&self) -> Option<Vec<Connection>> {
        let ptr = self.registrar.upgrade()?;
        let registrar = ptr.read();
        let item = registrar.toolset.find_item_template(&self.id)?;
        item.connections.clone()
    }

    pub fn is_group(&self) -> Option<bool> {
        let ptr = self.registrar.upgrade()?;
        let registrar = ptr.read();
        let item = registrar.toolset.find_item_template(&self.id)?;
        Some(item.is_group())
    }

    pub fn add_input(self, name: &str, ty: TypeSpec, default: Option<Value>) -> Option<PortTemplateID> {
        let ptr = self.registrar.upgrade()?;
        let mut registrar = ptr.write();
        let mut item = registrar.toolset.find_item_template(&self.id)?;
        Some(item.is_group())
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

    pub fn find_item_template_mut(&mut self, id: &TemplateID) -> Option<&mut ItemTemplate> {
        self.node_templates.get_mut(id)
    }

    pub fn find_item_instance(&self, id: &InstanceID) -> Option<&ItemInstance> {
        self.node_templates
            .values()
            .find_map(|template| template.item_instance_from_id(id))
    }

    pub fn find_item_instance_mut(&mut self, id: &InstanceID) -> Option<&mut ItemInstance> {
        self.node_templates
            .values()
            .find_map(|template| template.item_instance_from_id_mut(id))
    }

    pub fn find_port_template(&self, id: &PortTemplateID) -> Option<&PortTemplate> {
        self.node_templates
            .values()
            .find_map(|template| template.port_template_from_id(id))
    }

    pub fn find_port_template_mut(&mut self, id: &PortTemplateID) -> Option<&mut PortTemplate> {
        self.node_templates
            .values()
            .find_map(|template| template.port_template_from_id_mut(id))
    }

    pub fn find_port_instance(&self, id: &PortInstanceID) -> Option<&PortInstance> {
        self.node_templates
            .values()
            .find_map(|template| {
                template.nodes.as_ref()?.values().find_map(|inst| inst.ports.get_mut(id))
            })
    }

    pub fn find_port_instance_mut(&mut self, id: &PortInstanceID) -> Option<&mut PortInstance> {
        self.node_templates
            .values()
            .find_map(|template| {
                template.nodes.as_ref()?.values().find_map(|inst| inst.ports.get_mut(id))
            })
    }
}

pub struct ToolsetReadGuard {
    guard: ArcRwLockReadGuard<RawRwLock, Registrar>,
}

impl std::ops::Deref for ToolsetReadGuard {
    type Target = Toolset;
    fn deref(&self) -> &Self::Target {
        &self.guard.toolset
    }
}

pub struct ToolsetWriteGuard {
    guard: ArcRwLockWriteGuard<RawRwLock, Registrar>,
}

impl std::ops::Deref for ToolsetWriteGuard {
    type Target = Toolset;
    fn deref(&self) -> &Self::Target {
        &self.guard.toolset
    }
}

impl std::ops::DerefMut for ToolsetWriteGuard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard.toolset
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

pub fn read_toolset(registrar: &Arc<RwLock<Registrar>>) -> ToolsetReadGuard {
    ToolsetReadGuard {
        guard: registrar.read_arc(),
    }
}

pub fn write_toolset(registrar: &Arc<RwLock<Registrar>>) -> ToolsetWriteGuard {
    ToolsetWriteGuard {
        guard: registrar.write_arc(),
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

    use crate::ir::*;

    #[test]
    fn creating_template() {
        let registrar = Arc::new(RwLock::new(Registrar::new()));

        let main_group = {
            let mut reg = registrar.write();
            let main_group = ItemTemplate::new_group("My group");
            let id = main_group.id;
            reg.toolset.register_item_template(main_group);
            ItemTemplateRef::new(Arc::downgrade(&registrar), &id.clone())
        };

    }
}

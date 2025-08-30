// indexmap = "1.10"   // preserves insertion order (useful for deterministic behavior)

use indexmap::IndexMap;
use rbtrack_types::{Shape, TypeSpec, Value};
use std::{
    any::TypeId,
    collections::{HashMap, HashSet, VecDeque},
};
use uuid::Uuid;

/// -------------------------
/// Basic metadata & IDs
/// -------------------------
#[derive(Clone, Debug)]
pub struct NodeMeta {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
}

impl NodeMeta {
    pub fn new(name: impl Into<String>, description: impl Into<Option<String>>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            description: description.into(),
        }
    }
}

/// -------------------------
/// Diagnostics
/// -------------------------
#[derive(Clone, Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    /// Optionally point to a template/node/port (by id) for precise UI mapping
    pub related_ids: Vec<Uuid>,
}

/// -------------------------
/// Ports
/// -------------------------
#[derive(Clone, Debug, PartialEq)]
pub enum PortDirection {
    Input,
    Output,
}

#[derive(Clone, Debug)]
pub struct PortTemplate {
    pub id: Uuid,
    pub name: String,
    pub direction: PortDirection,
    pub ty: TypeSpec,
    /// For input ports: default value used when dangling (optional)
    pub default: Option<Value>,
}

impl PortTemplate {
    pub fn new_input(name: &str, ty: TypeSpec, default: Option<Value>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.to_string(),
            direction: PortDirection::Input,
            ty,
            default,
        }
    }
    pub fn new_output(name: &str, ty: TypeSpec) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.to_string(),
            direction: PortDirection::Output,
            ty,
            default: None,
        }
    }
}

/// -------------------------
/// Node templates & groups
/// -------------------------
#[derive(Clone, Debug)]
/// NodeTemplateKind:
pub enum NodeTemplateKind {
    /// - Atomic: a leaf node implemented by user/runtime
    Atomic,
    /// - Group: a node that refers to a group template (by id)
    Group(Uuid), // group template id
}

#[derive(Clone, Debug)]
pub struct NodeTemplate {
    pub meta: NodeMeta,
    pub ports: Vec<PortTemplate>,
    pub kind: NodeTemplateKind,
    // Optional: validation callback or capabilities descriptor (left runtime/plugin-bound)
}

impl NodeTemplate {
    pub fn new_atomic(name: &str, description: Option<&str>, ports: Vec<PortTemplate>) -> Self {
        Self {
            meta: NodeMeta::new(name, description.map(|s| s.to_string())),
            ports,
            kind: NodeTemplateKind::Atomic,
        }
    }

    pub fn new_group_like(name: &str, description: Option<&str>, group_id: Uuid) -> Self {
        let kind = NodeTemplateKind::Group(group_id);
        Self {
            meta: NodeMeta::new(name, description.map(|s| s.to_string())),
            ports: Vec::new(), // group-level ports stored on GroupTemplate
            kind,
        }
    }

    /// Find a port id by name. Optionally restrict to Input or Output via `dir`.
    pub fn find_port_id_by_name(&self, name: &str, dir: Option<PortDirection>) -> Option<Uuid> {
        for p in &self.ports {
            if p.name == name {
                if let Some(ref wanted_dir) = dir {
                    if &p.direction == wanted_dir {
                        return Some(p.id);
                    }
                } else {
                    return Some(p.id);
                }
            }
        }
        None
    }
}

/// GroupTemplate: defines a body graph and external ports (the group's interface)
#[derive(Clone, Debug)]
pub struct GroupTemplate {
    pub meta: NodeMeta,
    pub external_ports: Vec<PortTemplate>, // group's inputs/outputs
    /// Instances: mapping instance-id -> NodeInstanceTemplate (nodes inside the group)
    /// Use IndexMap for deterministic iteration order (useful for deterministic compilation)
    pub nodes: IndexMap<Uuid, NodeInstanceTemplate>,
    /// Connections internal to the group: from output (node, port id) to input (node, port id)
    pub connections: Vec<ConnectionTemplate>,
}

impl GroupTemplate {
    pub fn new(name: &str, description: Option<&str>, external_ports: Vec<PortTemplate>) -> Self {
        Self {
            meta: NodeMeta::new(name, description.map(|s| s.to_string())),
            external_ports,
            nodes: IndexMap::new(),
            connections: Vec::new(),
        }
    }
    pub fn resolve_port_ref<'a>(
        &'a self,
        node_id: Uuid,
        port_name: &str,
    ) -> Result<PortRef, String> {
        let node = self.nodes.get(&node_id)
            .ok_or_else(|| format!("Unknown node {node_id} in group"))?;

        let port = node.ports.iter()
            .find(|p| p.name == port_name)
            .ok_or_else(|| format!("Unknown port {port_name} on node {node_id}"))?;

        Ok(PortRef {
            node_id,
            port_name: port.name.clone(),
            port_type: port.port_type,
        })
    }

    pub fn add_node(&mut self, instance: NodeInstanceTemplate) {
        self.nodes.insert(instance.instance_id, instance);
    }

    pub fn add_connection(&mut self, conn: ConnectionTemplate) {
        self.connections.push(conn);
    }
}

/// A node *instance* inside a group template (refers to a NodeTemplate id)
#[derive(Clone, Debug)]
pub struct NodeInstanceTemplate {
    pub instance_id: Uuid,
    pub template_id: Uuid, // NodeTemplate id
    pub name_hint: Option<String>,
}

/// A connection inside a group template (all refs are instance-level)
#[derive(Clone, Debug)]
pub struct PortRef {
    pub node_id: Uuid, // if node_id == special GROUP_ID => refers to group external ports (we'll use group-level sentinel)
    pub port_id: Uuid, // the port template id (local to the referenced node or group)
}

const GROUP_PORT_SENTINEL: Uuid = Uuid::from_u128(0); // sentinel to represent group-external port refs

#[derive(Clone, Debug)]
pub struct ConnectionTemplate {
    pub id: Uuid,
    pub from: PortRef, // must be an output
    pub to: PortRef,   // must be an input
}

impl ConnectionTemplate {
    pub fn new(from: PortRef, to: PortRef) -> Self {
        Self {
            id: Uuid::new_v4(),
            from,
            to,
        }
    }
}

/// -------------------------
/// Toolset & Registrar
/// -------------------------
#[derive(Clone, Debug)]
pub struct Toolset {
    pub node_templates: IndexMap<Uuid, NodeTemplate>,
    pub group_templates: IndexMap<Uuid, GroupTemplate>,
}

impl Toolset {
    pub fn new() -> Self {
        Self {
            node_templates: IndexMap::new(),
            group_templates: IndexMap::new(),
        }
    }

    pub fn register_node_template(&mut self, template: NodeTemplate) -> Uuid {
        let id = template.meta.id;
        self.node_templates.insert(id, template);
        id
    }

    pub fn register_group_template(&mut self, group: GroupTemplate) -> Uuid {
        let id = group.meta.id;
        self.group_templates.insert(id, group);
        id
    }

    /// NEW:
    /// Find a port id (Uuid) inside a node template by port name.
    pub fn lookup_port_id_in_node_template(
        &self,
        template_id: Uuid,
        port_name: &str,
    ) -> Option<Uuid> {
        self.node_templates
            .get(&template_id)
            .and_then(|nt| nt.ports.iter().find(|p| p.name == port_name).map(|p| p.id))
    }

    /// NEW:
    /// Find a port id (Uuid) inside a group template's external ports by port name.
    pub fn lookup_port_id_in_group_template(
        &self,
        group_id: Uuid,
        port_name: &str,
    ) -> Option<Uuid> {
        self.group_templates.get(&group_id).and_then(|g| {
            g.external_ports
                .iter()
                .find(|p| p.name == port_name)
                .map(|p| p.id)
        })
    }
}

/// Registrar: owns the toolset and entry point (root group)
#[derive(Clone, Debug)]
pub struct Registrar {
    pub toolset: Toolset,
    pub entry_group: Option<Uuid>, // GroupTemplate id
}

impl Registrar {
    pub fn new() -> Self {
        Self {
            toolset: Toolset::new(),
            entry_group: None,
        }
    }

    pub fn set_entry_point(&mut self, group_id: Uuid) {
        self.entry_group = Some(group_id);
    }

    /// Resolve a (node instance id or group sentinel) + port name into a PortRef.
    /// Returns a Diagnostic on failure to ease UI error reporting.
    pub fn resolve_port_ref(
        &self,
        group: &GroupTemplate,
        node_id: Uuid,
        port_name: &str,
    ) -> Result<PortRef, Diagnostic> {
        // group external port
        if node_id == GROUP_PORT_SENTINEL {
            if let Some(p) = group.external_ports.iter().find(|p| p.name == port_name) {
                return Ok(PortRef {
                    node_id: GROUP_PORT_SENTINEL,
                    port_id: p.id,
                });
            } else {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Group '{}' has no external port named '{}'",
                        group.meta.name, port_name
                    ),
                    related_ids: vec![group.meta.id],
                });
            }
        }

        // find the NodeInstanceTemplate
        if let Some(inst) = group.nodes.get(&node_id) {
            // lookup node template
            if let Some(nt) = self.toolset.node_templates.get(&inst.template_id) {
                // find port on template
                if let Some(pid) = nt.find_port_id_by_name(port_name, None) {
                    return Ok(PortRef {
                        node_id,
                        port_id: pid,
                    });
                } else {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        message: format!(
                            "Node instance '{}' (template '{}') has no port named '{}'",
                            inst.instance_id, nt.meta.name, port_name
                        ),
                        related_ids: vec![inst.instance_id, nt.meta.id],
                    });
                }
            } else {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Node template '{}' (for instance {}) not found in toolset",
                        inst.template_id, inst.instance_id
                    ),
                    related_ids: vec![inst.instance_id, inst.template_id],
                });
            }
        } else {
            return Err(Diagnostic {
                severity: Severity::Error,
                message: format!("Node instance {} not found in group {}", node_id, group.meta.name),
                related_ids: vec![node_id, group.meta.id],
            });
        }
    }

    /// Convenience to add a connection by node/port names.
    /// `from_node` and `to_node` may be GROUP_PORT_SENTINEL to refer to group external ports.
    /// Returns Ok(connection_id) or Err(diagnostic) (the function will not mutate if it fails).
    pub fn add_connection_by_names(
        &mut self,
        group_id: Uuid,
        from_node: Uuid,
        from_port_name: &str,
        to_node: Uuid,
        to_port_name: &str,
    ) -> Result<Uuid, Diagnostic> {
        // get group mutably
        let group = match self.toolset.group_templates.get_mut(&group_id) {
            Some(g) => g,
            None => {
                return Err(Diagnostic {
                    severity: Severity::Error,
                    message: format!("Group template {} not found", group_id),
                    related_ids: vec![group_id],
                })
            }
        };

        // resolve both ends using non-mutable path (we only need &GroupTemplate)
        let from_ref = match self.resolve_port_ref(&*group, from_node, from_port_name) {
            Ok(r) => r,
            Err(d) => return Err(d),
        };
        let to_ref = match self.resolve_port_ref(&*group, to_node, to_port_name) {
            Ok(r) => r,
            Err(d) => return Err(d),
        };

        // Basic direction sanity check: ensure resolved templates have matching directions
        // Find PortTemplate for both to check direction
        let get_port_template = |node_id: Uuid, port_id: Uuid|
         -> Option<PortTemplate> {
            if node_id == GROUP_PORT_SENTINEL {
                group.external_ports.iter().find(|p| p.id == port_id).cloned()
            } else {
                group
                    .nodes
                    .get(&node_id)
                    .and_then(|inst| self.toolset.node_templates.get(&inst.template_id))
                    .and_then(|nt| nt.ports.iter().find(|p| p.id == port_id).cloned())
            }
        };

        let from_pt = get_port_template(from_ref.node_id, from_ref.port_id);
        let to_pt = get_port_template(to_ref.node_id, to_ref.port_id);

        match (&from_pt, &to_pt) {
            (Some(f), Some(t)) => {
                if f.direction != PortDirection::Output {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Resolved 'from' port '{}' is not an output", f.name),
                        related_ids: vec![f.id],
                    });
                }
                if t.direction != PortDirection::Input {
                    return Err(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Resolved 'to' port '{}' is not an input", t.name),
                        related_ids: vec![t.id],
                    });
                }
            }
            _ => {
                // one of the ports disappeared between resolve and now (unlikely), but handle gracefully
                return Err(Diagnostic {
                    severity: Severity::Error,
                    message: "Failed to fetch port templates after resolution".to_string(),
                    related_ids: vec![group.meta.id],
                });
            }
        }

        // All good — create connection and push to group's connections
        let conn = ConnectionTemplate::new(from_ref, to_ref);
        let conn_id = conn.id;
        group.add_connection(conn);
        Ok(conn_id)
    }

    /// Validate the whole graph starting at entry point.
    /// Collect diagnostics per-group and return map group_id -> diagnostics
    pub fn validate(&self) -> HashMap<Uuid, Vec<Diagnostic>> {
        // (existing validate impl unchanged)
        let mut diagnostics_map: HashMap<Uuid, Vec<Diagnostic>> = HashMap::new();

        if let Some(entry_id) = self.entry_group {
            // BFS/DFS through group templates reachable from entry to validate each
            let mut visited: HashSet<Uuid> = HashSet::new();
            let mut queue: VecDeque<Uuid> = VecDeque::new();
            queue.push_back(entry_id);

            while let Some(gid) = queue.pop_front() {
                if visited.contains(&gid) {
                    continue;
                }
                visited.insert(gid);

                if let Some(group) = self.toolset.group_templates.get(&gid) {
                    let diag = self.validate_group(group);
                    diagnostics_map.insert(gid, diag);

                    // discover referenced groups from node instances inside this group
                    for (_inst_id, inst) in group.nodes.iter() {
                        if let Some(node_t) = self.toolset.node_templates.get(&inst.template_id) {
                            match &node_t.kind {
                                NodeTemplateKind::Group(inner_gid) => {
                                    queue.push_back(*inner_gid);
                                }
                                _ => {}
                            }
                        } else {
                            // missing template diagnostic
                            diagnostics_map.entry(gid).or_default().push(Diagnostic {
                                severity: Severity::Error,
                                message: format!(
                                    "Node instance template missing: {}",
                                    inst.template_id
                                ),
                                related_ids: vec![inst.instance_id, gid],
                            });
                        }
                    }
                } else {
                    diagnostics_map.entry(gid).or_default().push(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Referenced group template not found: {}", gid),
                        related_ids: vec![gid],
                    });
                }
            }
        } else {
            // no entry point
            diagnostics_map.insert(
                Uuid::nil(),
                vec![Diagnostic {
                    severity: Severity::Error,
                    message: "No entry group set in registrar".to_string(),
                    related_ids: vec![],
                }],
            );
        }

        diagnostics_map
    }

    /// Validate a single group template: type-check connections, detect cycles, check port directions and dangling inputs.
    fn validate_group(&self, group: &GroupTemplate) -> Vec<Diagnostic> {
        let mut diags: Vec<Diagnostic> = Vec::new();

        // Build quick maps: node_instance_id -> template, node_instance_id -> node template's ports map
        // Also treat group-level external ports as node with id == GROUP_PORT_SENTINEL
        let mut instance_template_map: HashMap<Uuid, &NodeTemplate> = HashMap::new();
        for (_inst_id, inst) in group.nodes.iter() {
            if let Some(nt) = self.toolset.node_templates.get(&inst.template_id) {
                instance_template_map.insert(inst.instance_id, nt);
            } else {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!("Missing node template for instance {}", inst.instance_id),
                    related_ids: vec![inst.instance_id, inst.template_id],
                });
            }
        }

        // helper to find a PortTemplate by node id + port id
        let find_port = |node_id: Uuid,
                         port_id: Uuid|
         -> Option<(
            &PortTemplate,
            Uuid, /*owner template id for more context*/
        )> {
            if node_id == GROUP_PORT_SENTINEL {
                // group-level port
                group
                    .external_ports
                    .iter()
                    .find(|p| p.id == port_id)
                    .map(|p| (p, group.meta.id))
            } else {
                instance_template_map.get(&node_id).and_then(|nt| {
                    nt.ports
                        .iter()
                        .find(|p| p.id == port_id)
                        .map(|p| (p, nt.meta.id))
                })
            }
        };

        // Validate connections: direction, existance, and type compatibility
        for conn in group.connections.iter() {
            // Check from & to port exist
            let from_port_opt = find_port(conn.from.node_id, conn.from.port_id);
            let to_port_opt = find_port(conn.to.node_id, conn.to.port_id);

            if from_port_opt.is_none() {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Connection from-port not found in group {}: {:?}",
                        group.meta.name, conn.from
                    ),
                    related_ids: vec![conn.id, group.meta.id],
                });
                continue;
            }
            if to_port_opt.is_none() {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Connection to-port not found in group {}: {:?}",
                        group.meta.name, conn.to
                    ),
                    related_ids: vec![conn.id, group.meta.id],
                });
                continue;
            }

            let (from_port, from_owner_template) = from_port_opt.unwrap();
            let (to_port, to_owner_template) = to_port_opt.unwrap();

            // check that 'from' is an output and 'to' is an input
            if from_port.direction != PortDirection::Output {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Connection 'from' port is not an output: {} (owner template {})",
                        from_port.name, from_owner_template
                    ),
                    related_ids: vec![from_port.id, conn.id],
                });
            }
            if to_port.direction != PortDirection::Input {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Connection 'to' port is not an input: {} (owner template {})",
                        to_port.name, to_owner_template
                    ),
                    related_ids: vec![to_port.id, conn.id],
                });
            }

            // type compatibility: base type must match; shape rules:
            // - scalar -> scalar OK if same base
            // - scalar -> vector OK (broadcast)
            // - vector -> vector OK if base same and (either vector length matches or either is variable-length)
            // - vector -> scalar NOT OK (unless vector length == 1 or you allow reduction — we treat as error here)
            let from_ty = &from_port.ty;
            let to_ty = &to_port.ty;

            if from_ty.base != to_ty.base {
                diags.push(Diagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "Type base mismatch: {} -> {}",
                        format_type(from_ty),
                        format_type(to_ty)
                    ),
                    related_ids: vec![from_port.id, to_port.id, conn.id],
                });
            } else {
                // base same, check shape compatibility
                match (&from_ty.shape, &to_ty.shape) {
                    (Shape::Scalar, Shape::Scalar) => { /* ok */ }
                    (Shape::Scalar, Shape::Vector(_)) => { /* broadcast OK */ }
                    (Shape::Vector(flen), Shape::Vector(tlen)) => {
                        // OK if either target is variable-length, or both equal, or source variable length -> target fixed ambiguous; we allow if equal or target variable
                        let ok = match (flen, tlen) {
                            (_, None) => true,       // target variable => ok
                            (None, Some(_)) => true, // source variable => assume runtime N fits; allow
                            (Some(a), Some(b)) => a == b,
                        };
                        if !ok {
                            diags.push(Diagnostic {
                                severity: Severity::Error,
                                message: format!(
                                    "Vector length mismatch: {} -> {}",
                                    format_type(from_ty),
                                    format_type(to_ty)
                                ),
                                related_ids: vec![from_port.id, to_port.id, conn.id],
                            });
                        }
                    }
                    (Shape::Vector(_), Shape::Scalar) => {
                        diags.push(Diagnostic {
                            severity: Severity::Error,
                            message: format!(
                                "Cannot connect vector -> scalar without reduction: {} -> {}",
                                format_type(from_ty),
                                format_type(to_ty)
                            ),
                            related_ids: vec![from_port.id, to_port.id, conn.id],
                        });
                    }
                }
            }
        }

        // Dangling input ports: allowed — just ensure they exist (they do), optionally check default types are compatible
        // for ext_port in &group.external_ports {
        //     if ext_port.direction == PortDirection::Input {
        //         // if default exists, ensure default's type matches port type
        //         if let Some(default_val) = &ext_port.default {
        //             if !value_matches_type(default_val, &ext_port.ty) {
        //                 diags.push(Diagnostic {
        //                     severity: Severity::Error,
        //                     message: format!("Default value type mismatch for group external port {} ({})", ext_port.name, format_type(&ext_port.ty)),
        //                     related_ids: vec![ext_port.id, group.meta.id],
        //                 });
        //             }
        //         }
        //     }
        // }

        // Detect cycles in the flattened dependency graph implied by connections (simple topological check)
        // Build adjacency over node instance ids (including GROUP_PORT_SENTINEL as needed)
        // For cycle detection we only consider internal node instances (not group port sentinel).
        let mut adj: HashMap<Uuid, Vec<Uuid>> = HashMap::new();
        let mut indeg: HashMap<Uuid, usize> = HashMap::new();

        // Initialize nodes
        for (node_id, _inst) in group.nodes.iter() {
            adj.insert(*node_id, Vec::new());
            indeg.insert(*node_id, 0);
        }

        for conn in &group.connections {
            // from.node -> to.node edge (ignore group-level sentinel edges for cycle detection)
            if conn.from.node_id != GROUP_PORT_SENTINEL && conn.to.node_id != GROUP_PORT_SENTINEL {
                adj.entry(conn.from.node_id)
                    .or_default()
                    .push(conn.to.node_id);
                *indeg.entry(conn.to.node_id).or_default() += 1;
            }
        }

        // Kahn's algorithm
        let mut q: VecDeque<Uuid> = indeg
            .iter()
            .filter(|(_, d)| **d == 0)
            .map(|(k, _)| *k)
            .collect();
        let mut visited_count = 0usize;
        while let Some(n) = q.pop_front() {
            visited_count += 1;
            if let Some(neis) = adj.get(&n) {
                for &m in neis {
                    if let Some(d) = indeg.get_mut(&m) {
                        *d -= 1;
                        if *d == 0 {
                            q.push_back(m);
                        }
                    }
                }
            }
        }
        if visited_count != adj.len() {
            diags.push(Diagnostic {
                severity: Severity::Error,
                message: "Cycle detected inside group template".to_string(),
                related_ids: vec![group.meta.id],
            });
        }

        diags
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

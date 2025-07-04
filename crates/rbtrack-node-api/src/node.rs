use super::item::Item;
use rbtrack_types::errors::*;
use anyhow::Result;

pub trait Node: Item {
    fn run(&mut self) -> Result<(), BTrackError>;
    fn compile(&mut self) -> Result<(), BTrackError>;
}

macro_rules! define_node {
    (
        $name:ident {
            $(
                $port_field:ident : $port_ty:ty = $port_name:literal
            ),* $(,)?
        }
    ) => {
        pub struct $name {
            $(
                pub $port_field: Port,
            )*
            pub ports: Vec<*mut Port>,
            pub name_map: HashMap<&'static str, usize>,
            pub id_map: HashMap<Uuid, usize>,
        }

        impl $name {
            pub fn new() -> Self {
                let mut ports_vec = vec![];
                let mut name_map = HashMap::new();
                let mut id_map = HashMap::new();
                $(
                    let mut p = Port::new::<$port_ty>($port_name, $id);
                    let ptr = &mut p as *mut _;
                    ports_vec.push(ptr);
                    name_map.insert($port_name, ports_vec.len() - 1);
                    id_map.insert($id, ports_vec.len() - 1);
                )*

                Self {
                    $(
                        $port_field: Port::new::<$port_ty>($port_name, $id),
                    )*
                    ports: ports_vec,
                    name_map,
                    id_map,
                }
            }

            pub fn port_by_name(&self, name: &str) -> Option<&Port> {
                self.name_map.get(name).map(|&i| unsafe { &*self.ports[i] })
            }

            pub fn port_by_id(&self, id: Uuid) -> Option<&Port> {
                self.id_map.get(&id).map(|&i| unsafe { &*self.ports[i] })
            }

            pub fn port_by_index(&self, index: usize) -> Option<&Port> {
                self.ports.get(index).map(|&p| unsafe { &*p })
            }

            $(
                pub fn $port_field(&self) -> &Port {
                    &self.$port_field
                }

                paste::paste! {
                    pub fn [<$port_field _mut>](&mut self) -> &mut Port {
                        &mut self.$port_field
                    }
                }
            )*
        }
    };
}
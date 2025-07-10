use rbtrack_types::Variant;
use super::Item;
use rbtrack_types::errors::ConnectionError;
use rbtrack_types::sync::{Arc,RwLock,Weak};

pub trait Port: Send + Item {
    fn get_value(&self) -> Arc<RwLock<Variant>>;
}

pub struct Input {
    id:uuid::Uuid,
    label:String,
    desc:String,
    value:Arc<RwLock<Variant>>,
    parent_id:Option<uuid::Uuid>,
    connection:Option<Weak<RwLock<Output>>>,
}

impl Input {
    pub fn new(label:String, default_value:Variant, desc:Option<String>, parent_id:Option<uuid::Uuid>) -> Self {
        Self{
            id:uuid::Uuid::new_v4(),
            label:label,
            desc:desc.unwrap_or_default(),
            value:Arc::new(RwLock::new(default_value)),
            parent_id:parent_id,
            connection:None
        }
    }
    pub fn connect(&mut self, other: &Arc<RwLock<Output>>) -> Result<(), ConnectionError> {
        if let Some(weak) = &self.connection {
            if let Some(existing) = weak.upgrade() {
                if Arc::ptr_eq(&existing, other) {
                    return Err(ConnectionError::AlreadyConnected);
                }
            }
        }

        self.connection = Some(Arc::downgrade(other));
        Ok(())
    }
    pub fn disconnect(&mut self, other:&Arc<RwLock<Output>>) -> Result<(), ConnectionError> {
        if let Some(weak) = &self.connection {
            if let Some(existing) = weak.upgrade() {
                if Arc::ptr_eq(&existing, other) {
                    self.connection = None;
                    return Ok(());
                }
            }
        }

        Err(ConnectionError::NotConnected)
    }
    pub fn disconnect_all(&mut self) -> Result<(), ConnectionError> {
        if let Some(_) = &self.connection {
            self.connection = None;
            return Ok(())
        }
        Err(ConnectionError::NotConnected)
    }
}

impl Clone for Input {
    fn clone(&self) -> Self {
        Self{
            id:uuid::Uuid::new_v4(),
            label:self.label.clone(),
            desc:self.desc.clone(),
            value:Arc::new(RwLock::new(self.value.read().clone())),
            parent_id:None,
            connection:None,
        }
    }
}

impl Item for Input {
    fn desc(&self) -> &String {
        &self.desc
    }
    fn desc_mut(&mut self) -> &mut String {
        &mut self.desc
    }
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
    fn parent_id(&self) -> &Option<uuid::Uuid> {
        &self.parent_id
    }
    fn parent_id_mut(&mut self) -> &mut Option<uuid::Uuid> {
        &mut self.parent_id
    }
}

impl Port for Input {
    fn get_value(&self) -> Arc<RwLock<Variant>> {
        if let Some(weak) = &self.connection {
            if let Some(existing) = weak.upgrade() {
                return existing.read().get_value()
            }
        }
        return self.value.clone()
    }
}


pub struct Output {
    id:uuid::Uuid,
    label:String,
    desc:String,
    value:Arc<RwLock<Variant>>,
    parent_id:Option<uuid::Uuid>,
}

impl Output {
    pub fn new(label:String, default_value:Variant, desc:Option<String>, parent_id:Option<uuid::Uuid>) -> Self {
        Self{
            id:uuid::Uuid::new_v4(),
            label:label,
            desc:desc.unwrap_or_default(),
            value:Arc::new(RwLock::new(default_value)),
            parent_id:parent_id,
        }
    }
}

impl Item for Output {
    fn desc(&self) -> &String {
        &self.desc
    }
    fn desc_mut(&mut self) -> &mut String {
        &mut self.desc
    }
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
    fn parent_id(&self) -> &Option<uuid::Uuid> {
        &self.parent_id
    }
    fn parent_id_mut(&mut self) -> &mut Option<uuid::Uuid> {
        &mut self.parent_id
    }
}

impl Port for Output {
    fn get_value(&self) -> Arc<RwLock<Variant>> {
        self.value.clone()
    }
}
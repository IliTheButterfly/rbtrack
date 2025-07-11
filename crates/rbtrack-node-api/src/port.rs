use rbtrack_types::Variant;
use super::{Item,ItemInfo,ItemDesc,ItemID,ItemIndex,ItemLabel};
use rbtrack_types::errors::ConnectionError;
use rbtrack_types::sync::{Arc,RwLock,Weak};

pub trait Port: Send + Item {
    fn get_value(&self) -> Arc<RwLock<Variant>>;
    fn read(&self) -> parking_lot::ArcRwLockReadGuard<parking_lot::RawRwLock, Variant> {
        self.get_value().read_arc()
    }
    fn write(&self) -> parking_lot::ArcRwLockWriteGuard<parking_lot::RawRwLock, Variant> {
        self.get_value().write_arc()
    }
}


pub struct Input {
    info:ItemInfo,
    value:Arc<RwLock<Variant>>,
    connection:Option<Weak<RwLock<Output>>>,
}

impl Input {
    pub fn new(label:ItemLabel, default_value:Variant, desc:Option<ItemDesc>, parent_id:Option<ItemID>) -> Self {
        Self{
            info:ItemInfo {
                id: ItemID::new_v4(),
                label: label,
                desc: desc.unwrap_or_default(),
                parent_id: parent_id
            },
            value:Arc::new(RwLock::new(default_value)),
            connection:None,
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
    pub fn connection(&self) -> Option<Weak<RwLock<Output>>> {
        self.connection.clone()
    }
}

impl Item for Input {
    fn info(&self) -> &ItemInfo {
        &self.info
    }
    fn info_mut(&mut self) -> &mut ItemInfo {
        &mut self.info
    }
}

impl Clone for Input {
    fn clone(&self) -> Self {
        Self{
            info:ItemInfo{
                id:ItemID::new_v4(),
                label:self.info.label.clone(),
                desc:self.info.desc.clone(),
                parent_id:self.info.parent_id.clone(),
            },
            value:Arc::new(RwLock::new(self.value.read().clone())),
            connection:None,
        }
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
    info:ItemInfo,
    value:Arc<RwLock<Variant>>,
}

impl Output {
    pub fn new(label:String, default_value:Variant, desc:Option<String>, parent_id:Option<uuid::Uuid>) -> Self {
        Self{
            info:ItemInfo {
                id: ItemID::new_v4(),
                label: label,
                desc: desc.unwrap_or_default(),
                parent_id: parent_id
            },
            value:Arc::new(RwLock::new(default_value)),
        }
    }
}

impl Item for Output {
    fn info(&self) -> &ItemInfo {
        &self.info
    }
    fn info_mut(&mut self) -> &mut ItemInfo {
        &mut self.info
    }
}

impl Clone for Output {
    fn clone(&self) -> Self {
        Self{
            info:ItemInfo{
                id:ItemID::new_v4(),
                label:self.info.label.clone(),
                desc:self.info.desc.clone(),
                parent_id:self.info.parent_id.clone(),
            },
            value:Arc::new(RwLock::new(self.value.read().clone())),
        }
    }
}

impl Port for Output {
    fn get_value(&self) -> Arc<RwLock<Variant>> {
        self.value.clone()
    }
}
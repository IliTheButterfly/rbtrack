use uuid::Uuid;

pub type ItemIndex = usize;
pub type ItemID = Uuid;
pub type ItemLabel = String;
pub type ItemDesc = String;

#[derive(Clone)]
pub struct ItemInfo {
    pub id:ItemID,
    pub label:ItemLabel,
    pub desc:ItemDesc,
    pub parent_id:Option<ItemID>,
}

pub trait Item {
    fn info(&self) -> &ItemInfo;
    fn info_mut(&mut self) -> &mut ItemInfo;
}
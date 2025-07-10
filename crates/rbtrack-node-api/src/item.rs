use uuid::Uuid;

pub trait Item: Send + Sync {
    fn label(&self) -> &String;
    fn label_mut(&mut self) -> &mut String;
    fn desc(&self) -> &String;
    fn desc_mut(&mut self) -> &mut String;
    fn id(&self) -> &Uuid;
    fn id_mut(&mut self) -> &mut Uuid;
    fn parent_id(&self) -> &Option<Uuid>;
    fn parent_id_mut(&mut self) -> &mut Option<Uuid>;
}

impl Item for Box<dyn Item + '_> {
    fn label(&self) -> &String {
        (**self).label()
    }
    fn label_mut(&mut self) -> &mut String {
        (**self).label_mut()
    }
    fn desc(&self) -> &String {
        (**self).desc()
    }
    fn desc_mut(&mut self) -> &mut String {
        (**self).desc_mut()
    }
    fn id(&self) -> &Uuid {
        (**self).id()
    }
    fn id_mut(&mut self) -> &mut Uuid {
        (**self).id_mut()
    }
    fn parent_id(&self) -> &Option<Uuid> {
        (**self).parent_id()
    }
    fn parent_id_mut(&mut self) -> &mut Option<Uuid> {
        (**self).parent_id_mut()
    }
}
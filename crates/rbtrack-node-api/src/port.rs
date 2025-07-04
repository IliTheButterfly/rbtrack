use rbtrack_types::Variant;
use super::item::Item;

pub trait Port: Send + Item {
    fn get_value(&self) -> &Variant;
    fn get_value_mut(&mut self) -> &mut Variant;
}

pub trait Input: Port {
    fn connect_to(&mut self, other: &mut Box<dyn Output>);
}

pub trait Output: Port {
    fn connect_to(&mut self, other: &mut Box<dyn Input>);
}

pub trait InnerInput: Output {
    fn connect_to(&mut self, other: &mut Box<dyn OutterInput>);
}

pub trait InnerOutput: Input {
    fn connect_to(&mut self, other: &mut Box<dyn OutterOutput>);
}

pub trait OutterInput: Input {
    fn connect_to(&mut self, other: &mut Box<dyn InnerInput>);
}

pub trait OutterOutput: Output {
    fn connect_to(&mut self, other: &mut Box<dyn InnerOutput>);
}

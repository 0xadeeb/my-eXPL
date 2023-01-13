#[derive(Debug)]
pub struct Label {
    counter: u16,
}
impl Label {
    pub fn default() -> Self {
        Self { counter: 0 }
    }
    pub fn get(&mut self) -> u16 {
        let label = self.counter;
        self.counter += 1;
        label
    }
}

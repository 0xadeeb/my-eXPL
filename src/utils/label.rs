#[derive(Debug)]
pub struct LabelGenerator {
    counter: u16,
}
impl LabelGenerator {
    pub fn default() -> Self {
        Self { counter: 0 }
    }
    pub fn get(&mut self) -> u16 {
        let label = self.counter;
        self.counter += 1;
        label
    }
}

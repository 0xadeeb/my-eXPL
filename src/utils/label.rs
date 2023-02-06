#[derive(Debug)]
pub struct LabelGenerator {
    counter: i16,
}
impl LabelGenerator {
    pub fn default() -> Self {
        Self { counter: 1 }
    }
    pub fn get(&mut self) -> i16 {
        let label = self.counter;
        self.counter += 1;
        label
    }
}

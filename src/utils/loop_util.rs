#[derive(Debug)]
pub struct LoopStack {
    stack: Vec<(i16, i16)>,
}

impl Default for LoopStack {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl LoopStack {
    pub fn top(&self) -> Option<&(i16, i16)> {
        self.stack.last()
    }
    pub fn pop(&mut self) -> Option<(i16, i16)> {
        self.stack.pop()
    }
    pub fn push(&mut self, labels: (i16, i16)) {
        self.stack.push(labels);
    }
    pub fn condition_label(&mut self) -> Option<&i16> {
        self.stack.last().map(|(l, _)| l)
    }
    pub fn exit_label(&mut self) -> Option<&i16> {
        self.stack.last().map(|(_, l)| l)
    }
}

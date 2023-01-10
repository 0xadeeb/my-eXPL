use super::code_gen::*;
use lrpar::Span;

#[derive(Debug)]
pub struct RegisterPool {
    available: [bool; 20],
}

impl Default for RegisterPool {
    fn default() -> Self {
        Self {
            available: [true; 20],
        }
    }
}

impl RegisterPool {
    pub fn get_reg(&mut self) -> Option<u8> {
        for (i, reg) in self.available.iter_mut().enumerate() {
            if *reg {
                *reg = false;
                return Some(i as u8);
            }
        }
        None
    }

    pub fn free_reg(&mut self, reg: u8) {
        if reg < 20 {
            self.available[reg as usize] = true;
        }
    }

    pub fn accquired_regs<'t>(&'t self) -> impl Iterator<Item = u8> + DoubleEndedIterator + 't {
        self.available
            .iter()
            .enumerate()
            .filter(|(_, &v)| !v)
            .map(|(i, _)| i as u8)
    }
}

pub fn make_operator_node(op: Op, span: Span, left: Tnode, right: Tnode) -> Tnode {
    Tnode::Operator {
        op,
        span,
        left: Box::new(left),
        right: Box::new(right),
    }
}

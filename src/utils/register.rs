#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum RegState {
    Acquired,
    Free,
    RetAcquired,
}

#[derive(Debug)]
pub struct RegisterPool {
    state: [RegState; 20],
    stored: Vec<[RegState; 20]>,
}

impl Default for RegisterPool {
    fn default() -> Self {
        Self {
            state: [RegState::Free; 20],
            stored: Vec::new(),
        }
    }
}

impl RegisterPool {
    fn get(&mut self, state: RegState) -> Option<u8> {
        self.state
            .iter_mut()
            .enumerate()
            .find(|(_, &mut v)| v == RegState::Free)
            .map(|(i, reg)| {
                *reg = state;
                i as u8
            })
    }

    pub fn get_reg(&mut self) -> Option<u8> {
        self.get(RegState::Acquired)
    }

    pub fn get_ret_reg(&mut self) -> Option<u8> {
        self.get(RegState::RetAcquired)
    }

    pub fn free_reg(&mut self, reg: u8) {
        if reg < 20 {
            self.state[reg as usize] = RegState::Free;
        }
    }

    pub fn save_context<'t>(&'t mut self) -> impl Iterator<Item = u8> + DoubleEndedIterator + 't {
        self.stored.push(self.state);
        self.state
            .iter()
            .enumerate()
            .filter(|(_, &v)| v == RegState::Acquired)
            .map(|(i, _)| i as u8)
    }

    pub fn restore_context<'t>(
        &'t mut self,
    ) -> impl Iterator<Item = u8> + DoubleEndedIterator + 't {
        let it = self
            .stored
            .last()
            .unwrap()
            .clone()
            .into_iter()
            .enumerate()
            .filter(|(_, v)| *v == RegState::Acquired)
            .map(|(i, _)| i as u8);

        for i in 0..20 {
            if self.state[i] == RegState::RetAcquired {
                self.stored.last_mut().unwrap()[i] = RegState::Acquired;
            }
        }

        self.state = self.stored.pop().unwrap();
        it
    }
}

use mem;

pub enum Button {
    Right = 0,
    Left = 1,
    Up = 2,
    Down = 3,

    A = 4,
    B = 5,
    Select = 6,
    Start = 7,
}

enum RegSelect {
    Direction = 4,
    Buttons = 5,
}

#[derive(Default)]
pub struct Joypad {
    button_state: u8,
}

impl Joypad {
    pub fn set_button(&mut self, button: Button, val: bool) {
        let mask = 1 << button as u8;
        self.button_state = if val {
            self.button_state | mask
        } else {
            self.button_state & !mask
        }
    }

    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        let joyp = mem.read_reg(mem::RegAddr::P1);

        let mut new_joyp = 0;

        if joyp & (1 << RegSelect::Direction as u8) == 0 {
            new_joyp |= self.button_state & 0xf;
        }

        if joyp & (1 << RegSelect::Buttons as u8) == 0 {
            new_joyp |= self.button_state >> 4;
        }

        mem.write_reg(mem::RegAddr::P1, new_joyp);
    }
}

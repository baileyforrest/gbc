use cpu;
use mem;

#[derive(Default)]
pub struct Timer {
    div_counter: u8,
    timer_counter: u16,
}

impl Timer {
    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        self.update_div(mem);
        self.update_timer(mem);
    }

    fn update_div(&mut self, mem: &mut mem::Mem) {
        self.div_counter = self.div_counter.overflowing_add(1).0;
        if self.div_counter == 0 {
            let div = mem.read_reg(mem::RegAddr::DIV);
            let (new_div, _) = div.overflowing_add(1);
            mem.write_reg(mem::RegAddr::DIV, new_div);
        }
    }

    fn update_timer(&mut self, mem: &mut mem::Mem) {
        self.timer_counter = self.timer_counter.overflowing_add(1).0;

        let tac = mem.read_reg(mem::RegAddr::TAC);
        let enabled = tac & 0x4 != 0;

        if !enabled {
            return;
        }

        let clock_sel = tac & 0x3;
        let div = match clock_sel {
            0x00 => 1024,
            0x01 => 16,
            0x02 => 64,
            0x03 => 256,
            _ => panic!("Impossible"),
        };

        if self.timer_counter % div != 0 {
            return;
        }

        let tima = mem.read_reg(mem::RegAddr::TIMA);
        let new_tima = if tima == 0xff {
            mem.set_interrupt_flag(cpu::Interrupt::Timer, true);
            mem.read_reg(mem::RegAddr::TMA)
        } else {
            tima + 1
        };
        mem.write_reg(mem::RegAddr::TIMA, new_tima);

        // TODO: Handle double speed mode. (divide div by 2)
    }
}

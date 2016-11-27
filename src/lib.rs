use std::fmt;
use std::io::Read;

mod cpu;
mod dma;
mod joypad;
mod lcd;
mod mem;
mod sound;
mod timer;

pub enum GbcErr {
    Cartridge(mem::CartErr),
}

impl fmt::Display for GbcErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &GbcErr::Cartridge(ref e) => write!(f, "Cartridge error: {}", e),
        }
    }
}

#[derive(Default)]
pub struct Gbc {
    cpu: cpu::Cpu,
    dma: dma::Dma,
    mem: mem::Mem,
    lcd: lcd::Lcd,
    timer: timer::Timer,
    joypad: joypad::Joypad,
    sound: sound::Sound,
}

impl Gbc {
    pub fn cpu_regs(&self) -> cpu::Regs {
        self.cpu.regs()
    }

    pub fn set_cpu_regs(&mut self, regs: cpu::Regs) {
        self.cpu.set_regs(regs);
    }

    pub fn mem_val(&self, addr: u16) -> u8 {
        self.mem.read(addr)
    }

    pub fn set_mem_val(&mut self, addr: u16, val: u8) {
        self.mem.write(addr, val);
    }

    pub fn get_frame_buffer(&self) -> &lcd::FrameBuffer {
        self.lcd.get_frame_buffer()
    }

    pub fn on_clock(&mut self) {
        self.cpu.on_clock(&mut self.mem);
        self.lcd.on_clock(&mut self.mem);
        self.timer.on_clock(&mut self.mem);
        self.joypad.on_clock(&mut self.mem);
        self.dma.on_clock(&mut self.mem);
    }

    pub fn load_rom(&mut self, rom: &mut Read) -> Result<(), GbcErr> {
        Ok(self.mem.load_cartridge(rom).map_err(GbcErr::Cartridge)?)
    }

    pub fn step_instruction(&mut self) {
        while self.cpu.inst_cycles() != 0 {
            self.on_clock();
        }
        self.on_clock();
    }
}

pub fn run() {
    let mut gbc: Gbc = Default::default();
    gbc.on_clock();
    println!("Hello, world!");
}

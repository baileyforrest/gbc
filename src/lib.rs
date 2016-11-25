mod cpu;
mod joypad;
mod lcd;
mod mem;
mod timer;

#[derive(Default)]
struct Gbc {
    cpu: cpu::Cpu,
    mem: mem::Mem,
    lcd: lcd::Lcd,
    timer: timer::Timer,
    joypad: joypad::Joypad,
}

impl Gbc {
    fn on_clock(&mut self) {
        self.cpu.on_clock(&mut self.mem);
        self.lcd.on_clock(&mut self.mem);
        self.timer.on_clock(&mut self.mem);
        self.joypad.on_clock(&mut self.mem);
    }
}

pub fn run() {
    let mut gbc: Gbc = Default::default();
    gbc.on_clock();
    println!("Hello, world!");
}

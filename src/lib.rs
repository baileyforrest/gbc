pub mod cpu;
pub mod mem;

#[derive(Default)]
struct Gbc {
    cpu: cpu::Cpu,
    mem: mem::Mem,
}

impl Gbc {
    fn on_clock(&mut self) {
        let mem_writes = self.cpu.on_clock(&self.mem);

        for &(addr, val) in &mem_writes {
            self.mem.write(addr, val);
        }
    }
}

pub fn run() {
    let mut gbc: Gbc = Default::default();
    gbc.on_clock();
    println!("Hello, world!");
}

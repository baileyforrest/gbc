pub mod cpu;
pub mod mem;

pub fn run() {
    let mut cpu = cpu::create();
    cpu.on_clock();
    println!("Hello, world!");
}

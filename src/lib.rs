pub mod cpu;
pub mod mem;

pub fn run() {
    let cpu = cpu::create();
    println!("Hello, world! {}", cpu.get_reg16(cpu::RegType16::PC));
}

struct Regs {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
}

pub struct Cpu {
    regs: Regs,
}

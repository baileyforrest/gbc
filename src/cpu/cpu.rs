use mem::mem;

const DEFAULT_PC: u16 = 0x100;
const DEFAULT_SP: u16 = 0xfffe;

#[derive(Default, Copy, Clone)]
struct Regs {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
    flags: u8,
    hl_indir: Option<u8>,
}

#[derive(Default)]
pub struct Cpu {
    regs: Regs,
    next_regs: Regs, // Register values after instruction completes.
    inst_cycles: u8, // Cycles remaining in current instruction.
    mem: mem::Mem,
    running: bool,
}

enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(PartialEq)]
enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    HL_INDIR, // Indirect HL
}

#[derive(Copy, Clone)]
enum FlagType {
    Z = 7, // Zero flag
    N = 6, // Subtract flag
    H = 5, // Half carry
    C = 4, // Carry
}

pub fn create() -> Cpu {
    Cpu {
        regs: Regs {
            sp: DEFAULT_SP,
            pc: DEFAULT_PC,
            ..Default::default()
        },
        ..Default::default()
    }
}

fn ridx_to_r8(idx: u8) -> Reg8 {
    match idx {
        0 => Reg8::B,
        1 => Reg8::C,
        2 => Reg8::D,
        3 => Reg8::E,
        4 => Reg8::H,
        5 => Reg8::L,
        6 => Reg8::HL_INDIR,
        7 => Reg8::A,
        _ => panic!("Invalid index"),
    }
}

impl Regs {
    fn get16(&self, rt: Reg16) -> u16 {
        match rt {
            Reg16::AF => self.af,
            Reg16::BC => self.bc,
            Reg16::DE => self.de,
            Reg16::HL => self.hl,
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    fn set16(&mut self, rt: Reg16, val: u16) {
        match rt {
            Reg16::AF => self.af = val,
            Reg16::BC => self.bc = val,
            Reg16::DE => self.de = val,
            Reg16::HL => self.hl = val,
            Reg16::SP => self.sp = val,
            Reg16::PC => self.pc = val,
        }
    }

    fn get8(&self, rt: Reg8) -> u8 {
        match rt {
            Reg8::A => u16_get_byte_high(self.af),
            Reg8::F => u16_get_byte_low(self.af),
            Reg8::B => u16_get_byte_high(self.bc),
            Reg8::C => u16_get_byte_low(self.bc),
            Reg8::D => u16_get_byte_high(self.de),
            Reg8::E => u16_get_byte_low(self.de),
            Reg8::H => u16_get_byte_high(self.hl),
            Reg8::L => u16_get_byte_low(self.hl),
            Reg8::HL_INDIR => panic!(),
        }
    }

    fn set8(&mut self, rt: Reg8, val: u8) {
        match rt {
            Reg8::A => u16_set_byte_high(&mut self.af, val),
            Reg8::F => u16_set_byte_low(&mut self.af, val),
            Reg8::B => u16_set_byte_high(&mut self.bc, val),
            Reg8::C => u16_set_byte_low(&mut self.bc, val),
            Reg8::D => u16_set_byte_high(&mut self.de, val),
            Reg8::E => u16_set_byte_low(&mut self.de, val),
            Reg8::H => u16_set_byte_high(&mut self.hl, val),
            Reg8::L => u16_set_byte_low(&mut self.hl, val),
            Reg8::HL_INDIR => self.hl_indir = Some(val),
        }
    }

    fn get_flag(&self, ft: FlagType) -> bool {
        ((self.flags >> ft as u8) & 0x1) == 0
    }

    fn set_flag(&mut self, ft: FlagType, val: bool) {
        let mask = !(1 << ft as u8);
        self.flags = (self.flags & mask) | ((val as u8) << (ft as u8));
    }

    fn flag_idx_pass(&self, idx: u8) -> bool {
        match idx {
            0 => (1 << FlagType::Z as u8) & self.flags == 0, // NZ
            1 => (1 << FlagType::Z as u8) & self.flags != 0, // Z
            2 => (1 << FlagType::C as u8) & self.flags == 0, // NC
            3 => (1 << FlagType::C as u8) & self.flags != 0, // C
            _ => panic!("Unsupported index"),
        }
    }
}

impl Cpu {
    pub fn on_clock(&mut self) {
        match self.inst_cycles {
            0 => self.run_inst(),
            1 => self.regs = self.next_regs,  // TODO: Set HL indir
            _ => self.inst_cycles -= 1,
        }
    }

    fn get_reg8(&self, r8: Reg8) -> u8 {
        match r8 {
            Reg8::HL_INDIR => self.mem.read(self.regs.get16(Reg16::HL)),
            _ => self.regs.get8(r8),
        }
    }

    // Helper function to get
    fn read_pc_val(&self, offset: i16) -> u8 {
        let signed_addr: i32 = self.regs.pc as i32 + offset as i32;
        assert!(signed_addr >= 0 && signed_addr <= u16::max_value() as i32,
                "PC out of range");

        self.mem.read(signed_addr as u16)
    }

    fn run_inst(&mut self) {
        let mut cycles: u8 = 4;
        let mut size: u16 = 1;

        self.next_regs = self.regs;

        let byte0 = self.read_pc_val(0);
        match byte0 {
            0xcb => panic!("unimplemented"),
            0xdd => panic!("unimplemented"),
            0xfd => panic!("unimplemented"),
            _ => {
                let x = (byte0 >> 6) & 0x3; // [7:6]
                let y = (byte0 >> 3) & 0x7; // [5:3]
                let z = (byte0 >> 0) & 0x7; // [2:0]
                let p = (byte0 >> 4) & 0x3; // [5:4]
                let q = (byte0 >> 3) & 0x1; // [3:3]

                match x {
                    0 => {
                        match z {
                            0 => {
                                match y {
                                    0 => {
                                        // NOP
                                    }
                                    1 => {
                                        // LD (a16),SP
                                        // TODO
                                    }
                                    2 => {
                                        // STOP
                                        // TODO
                                    }
                                    3...7 => {
                                        // y == 3: JR d
                                        // y > 3: JR cc[y-4], d
                                        size = 2;
                                        let cc_idx = y - 4;

                                        if y == 3 || self.regs.flag_idx_pass(cc_idx) {
                                            cycles = 12;
                                            let val = self.read_pc_val(1);
                                            // TODO: is it pc before or after current instruction?
                                            let next_pc = self.regs.get16(Reg16::PC) + val as u16;
                                            self.next_regs.set16(Reg16::PC, next_pc);
                                        } else {
                                            cycles = 8;
                                        }
                                    }
                                    _ => panic!("Impossible"),
                                }
                            }
                            6 => {
                                // LD r[y], n
                                size = 2;
                                cycles = 8;
                                let val = self.read_pc_val(1);
                                let reg = ridx_to_r8(y);
                                if reg == Reg8::HL_INDIR {
                                    cycles = 12;
                                }
                                self.next_regs.set8(reg, val);
                            }
                            _ => panic!("Not implemented."),
                        }
                    }

                    1 => {
                        if z == 6 && y == 6 {
                            // HALT
                            self.running = false;
                        } else {
                            // LD r[y], r[z]

                            let src = ridx_to_r8(z);
                            let dst = ridx_to_r8(y);
                            let src_val = self.get_reg8(src);
                            self.next_regs.set8(dst, src_val);
                        }
                    }
                    _ => panic!("Not implemented"),
                }
            }
        }

        self.inst_cycles = cycles;
        self.next_regs.pc += size;
    }
}

fn u16_get_byte_high(i: u16) -> u8 {
    ((i >> 8) & 0xff) as u8
}

fn u16_get_byte_low(i: u16) -> u8 {
    (i & 0xff) as u8
}

fn u16_set_byte_high(r: &mut u16, val: u8) {
    *r = (*r & 0xff) | ((val as u16) << 8)
}

fn u16_set_byte_low(r: &mut u16, val: u8) {
    *r = (*r & 0xff00) | val as u16;
}

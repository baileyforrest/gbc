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
}

#[derive(Default)]
pub struct Cpu {
    regs: Regs,
    next_regs: Regs, // Register values after instruction completes.
    inst_cycles: u8, // Cycles remaining in current instruction.
    mem: mem::Mem,
}

pub enum RegType16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

pub enum RegType8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Copy, Clone)]
pub enum FlagType {
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

impl Cpu {
    pub fn get_reg16(&self, rt: RegType16) -> u16 {
        match rt {
            RegType16::AF => self.regs.af,
            RegType16::BC => self.regs.bc,
            RegType16::DE => self.regs.de,
            RegType16::HL => self.regs.hl,
            RegType16::SP => self.regs.sp,
            RegType16::PC => self.regs.pc,
        }
    }

    pub fn set_16(&mut self, rt: RegType16, val: u16) {
        match rt {
            RegType16::AF => self.regs.af = val,
            RegType16::BC => self.regs.bc = val,
            RegType16::DE => self.regs.de = val,
            RegType16::HL => self.regs.hl = val,
            RegType16::SP => self.regs.sp = val,
            RegType16::PC => self.regs.pc = val,
        }
    }

    pub fn get_reg8(&self, rt: RegType8) -> u8 {
        match rt {
            RegType8::A => u16_get_byte_high(self.regs.af),
            RegType8::F => u16_get_byte_low(self.regs.af),
            RegType8::B => u16_get_byte_high(self.regs.bc),
            RegType8::C => u16_get_byte_low(self.regs.bc),
            RegType8::D => u16_get_byte_high(self.regs.de),
            RegType8::E => u16_get_byte_low(self.regs.de),
            RegType8::H => u16_get_byte_high(self.regs.hl),
            RegType8::L => u16_get_byte_low(self.regs.hl),
        }
    }

    pub fn set_reg8(&mut self, rt: RegType8, val: u8) {
        match rt {
            RegType8::A => u16_set_byte_high(&mut self.regs.af, val),
            RegType8::F => u16_set_byte_low(&mut self.regs.af, val),
            RegType8::B => u16_set_byte_high(&mut self.regs.bc, val),
            RegType8::C => u16_set_byte_low(&mut self.regs.bc, val),
            RegType8::D => u16_set_byte_high(&mut self.regs.de, val),
            RegType8::E => u16_set_byte_low(&mut self.regs.de, val),
            RegType8::H => u16_set_byte_high(&mut self.regs.hl, val),
            RegType8::L => u16_set_byte_low(&mut self.regs.hl, val),
        }
    }

    // Helper function to get
    pub fn read_pc_val(&self, offset: i16) -> u8 {
        let signed_addr: i32 = self.regs.pc as i32 + offset as i32;
        assert!(signed_addr >= 0 && signed_addr <= u16::max_value() as i32,
                "PC out of range");

        self.mem.read(signed_addr as u16)
    }

    pub fn get_flag(&self, ft: FlagType) -> bool {
        ((self.regs.flags >> ft as u8) & 0x1) == 0
    }

    pub fn set_flag(&mut self, ft: FlagType, val: bool) {
        let mask = !(1 << ft as u8);
        self.regs.flags = (self.regs.flags & mask) | ((val as u8) << (ft as u8));
    }

    pub fn on_clock(&mut self) {
        match self.inst_cycles {
            0 => self.run_inst(),
            1 => self.regs = self.next_regs,
            _ => self.inst_cycles -= 1,
        }
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

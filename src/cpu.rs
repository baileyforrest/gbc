use mem;
use std::fmt;

#[derive(Copy, Clone)]
pub enum Interrupt {
    Vblank = 0x40,
    LcdStatus = 0x48,
    Timer = 0x50,
    Serial = 0x58,
    Joypad = 0x60,
}

#[derive(Copy, Clone)]
pub struct Regs {
    pub af: u16,
    pub bc: u16,
    pub de: u16,
    pub hl: u16,
    pub sp: u16,
    pub pc: u16,
    pub enable_interrupts: bool,
    pub halted: bool,
    pub stopped: bool,
}

#[derive(Default)]
struct NextState {
    regs: Regs,
    mem_writes: Vec<(u16, u8)>, // Pending writes to memory.
    cycles: u8, // Cycles required for next transition.
}

struct NextStateGen<'a> {
    cpu: &'a Cpu,
    mem: &'a mem::Mem,
    ns: Box<NextState>,
}

#[derive(Default)]
pub struct Cpu {
    regs: Regs,
    inst_cycles: u8, // Cycles remaining in current instruction.

    next_state: Option<Box<NextState>>,
}

#[derive(Copy, Clone)]
enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Copy, Clone, PartialEq)]
enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI, // HL indirect
}

#[derive(Copy, Clone)]
enum FlagType {
    Z = 7, // Zero flag
    N = 6, // Subtract flag
    H = 5, // Half carry
    C = 4, // Carry
}

fn r_idx_to_r8(idx: u8) -> Reg8 {
    match idx {
        0 => Reg8::B,
        1 => Reg8::C,
        2 => Reg8::D,
        3 => Reg8::E,
        4 => Reg8::H,
        5 => Reg8::L,
        6 => Reg8::HLI,
        7 => Reg8::A,
        _ => panic!("Invalid index"),
    }
}

fn rp2_idx_to_r16(idx: u8) -> Reg16 {
    match idx {
        0 => Reg16::BC,
        1 => Reg16::DE,
        2 => Reg16::HL,
        3 => Reg16::AF,
        _ => panic!("Invalid index"),
    }
}

fn rp_idx_to_r16(idx: u8) -> Reg16 {
    match idx {
        0 => Reg16::BC,
        1 => Reg16::DE,
        2 => Reg16::HL,
        3 => Reg16::SP,
        _ => panic!("Invalid index"),
    }
}

impl Default for Regs {
    fn default() -> Regs {
        Regs {
            af: 0x11B0,
            bc: 0x0013,
            de: 0x00D8,
            hl: 0x014d,
            sp: 0xfffe,
            pc: 0x0100,
            enable_interrupts: false,
            halted: false,
            stopped: false,
        }
    }
}

impl fmt::Display for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "AF: 0x{:04x}\n", self.af)?;
        write!(f, "BC: 0x{:04x}\n", self.bc)?;
        write!(f, "DE: 0x{:04x}\n", self.de)?;
        write!(f, "HL: 0x{:04x}\n", self.hl)?;
        write!(f, "SP: 0x{:04x}\n", self.sp)?;
        write!(f, "PC: 0x{:04x}\n", self.pc)?;
        write!(f, "IME: {}\n", self.enable_interrupts)?;
        write!(f, "HALT: {}\n", self.halted)?;
        write!(f, "STOP: {}\n", self.stopped)
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
            Reg8::HLI => panic!(),
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
            Reg8::HLI => panic!("Can't set HL indirect"),
        }
    }

    fn get_flag(&self, ft: FlagType) -> bool {
        ((self.get8(Reg8::F) >> ft as u8) & 0x1) == 0
    }

    fn set_flag(&mut self, ft: FlagType, val: bool) {
        let mask = !(1 << ft as u8);
        let new_val = (self.get8(Reg8::F) & mask) | ((val as u8) << (ft as u8));
        self.set8(Reg8::F, new_val);
    }

    fn flag_idx_pass(&self, idx: u8) -> bool {
        let flags = self.get8(Reg8::F);
        match idx {
            0 => (1 << FlagType::Z as u8) & flags == 0, // NZ
            1 => (1 << FlagType::Z as u8) & flags != 0, // Z
            2 => (1 << FlagType::C as u8) & flags == 0, // NC
            3 => (1 << FlagType::C as u8) & flags != 0, // C
            _ => panic!("Unsupported index"),
        }
    }
}

impl Cpu {
    pub fn regs(&self) -> Regs {
        self.regs
    }

    pub fn set_regs(&mut self, regs: Regs) {
        self.regs = regs;
    }

    pub fn inst_cycles(&self) -> u8 {
        self.inst_cycles
    }

    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        match self.inst_cycles {
            0 => {
                // Apply next_state
                if let Some(ref next_state) = self.next_state {
                    self.regs = next_state.regs;

                    for &(addr, val) in &next_state.mem_writes {
                        mem.write(addr, val);
                    }
                }

                if self.regs.enable_interrupts {
                    for &intr in [Interrupt::Vblank,
                                  Interrupt::LcdStatus,
                                  Interrupt::Timer,
                                  Interrupt::Serial,
                                  Interrupt::Joypad]
                        .iter() {
                        if !mem.get_interrupt_en(intr) || !mem.get_interrupt_flag(intr) {
                            continue;
                        }

                        self.regs.halted = false;

                        // Process interrupt
                        mem.set_interrupt_flag(intr, false);
                        self.regs.enable_interrupts = false;

                        let next_state = {
                            let mut nsg = NextStateGen {
                                cpu: self,
                                mem: mem,
                                ns: Default::default(),
                            };
                            nsg.generate_interrupt(intr);
                            nsg.ns
                        };
                        self.inst_cycles = next_state.cycles - 1;
                        self.next_state = Some(next_state);
                        return;
                    }
                }

                // TODO: sleep screen. Wake when button pressed
                if self.regs.stopped {
                    self.next_state = None;
                    return;
                }

                if self.regs.halted {
                    self.next_state = None;
                    return;
                }

                // Generate next state
                let next_state = {
                    let mut nsg = NextStateGen {
                        cpu: self,
                        mem: mem,
                        ns: Default::default(),
                    };
                    nsg.generate();
                    nsg.ns
                };

                // Subtract one to count for this cycle.
                self.inst_cycles = next_state.cycles - 1;
                self.next_state = Some(next_state);
            }
            _ => self.inst_cycles -= 1,
        }
    }
}

impl<'a> NextStateGen<'a> {
    // Helper function to get values relative to pc.
    fn read_pc_val(&self, offset: i16) -> u8 {
        let signed_addr: i32 = self.cpu.regs.pc as i32 + offset as i32;
        assert!(signed_addr >= 0 && signed_addr <= u16::max_value() as i32,
                "PC out of range");

        self.mem.read(signed_addr as u16)
    }

    fn read_pc_val16(&self, offset: i16) -> u16 {
        let byte0 = self.read_pc_val(offset) as u16;
        let byte1 = self.read_pc_val(offset + 1) as u16;
        byte0 | byte1 << 8
    }

    fn get_reg8(&self, r8: Reg8) -> u8 {
        match r8 {
            Reg8::HLI => self.mem.read(self.cpu.regs.get16(Reg16::HL)),
            _ => self.cpu.regs.get8(r8),
        }
    }

    fn set_reg8(&mut self, r8: Reg8, val: u8) {
        match r8 {
            Reg8::HLI => self.ns.mem_writes.push((self.cpu.regs.get16(Reg16::HL), val)),
            _ => self.ns.regs.set8(r8, val),
        }
    }

    fn pop(&mut self) -> u16 {
        self.ns.regs.sp = self.cpu.regs.sp.overflowing_add(2).0;
        self.mem.read(self.cpu.regs.sp) as u16 | (self.mem.read(self.cpu.regs.sp + 1) as u16) << 8
    }

    fn push(&mut self, val: u16) {
        self.ns.regs.sp = self.cpu.regs.sp.overflowing_sub(2).0;
        self.ns.mem_writes.push((self.cpu.regs.sp.overflowing_sub(1).0, (val >> 8) as u8));
        self.ns.mem_writes.push((self.cpu.regs.sp.overflowing_sub(2).0, val as u8));
    }

    fn ret(&mut self) {
        self.ns.regs.pc = self.pop();
    }

    fn call(&mut self, addr: u16, ret_val: u16) {
        self.push(ret_val);
        self.ns.regs.pc = addr;
    }

    fn rlc(&mut self, r: Reg8) {
        let val = self.get_reg8(r);
        let new_val = val << 1 | val >> 7;

        self.ns.regs.set_flag(FlagType::Z, new_val == 0);
        self.ns.regs.set_flag(FlagType::N, false);
        self.ns.regs.set_flag(FlagType::H, false);

        let c = val & (1 << 7) != 0;
        self.ns.regs.set_flag(FlagType::C, c);
        self.set_reg8(r, new_val);
    }

    fn rrc(&mut self, r: Reg8) {
        let val = self.get_reg8(r);
        let new_val = val >> 1 | val << 7;

        self.ns.regs.set_flag(FlagType::Z, new_val == 0);
        self.ns.regs.set_flag(FlagType::N, false);
        self.ns.regs.set_flag(FlagType::H, false);

        let c = val & 0x1 != 0;
        self.ns.regs.set_flag(FlagType::C, c);
        self.set_reg8(r, new_val);
    }

    fn rl(&mut self, r: Reg8) {
        let val = self.get_reg8(r);
        let new_val = val << 1 | self.cpu.regs.get_flag(FlagType::C) as u8;

        self.ns.regs.set_flag(FlagType::Z, new_val == 0);
        self.ns.regs.set_flag(FlagType::N, false);
        self.ns.regs.set_flag(FlagType::H, false);

        let c = val & (1 << 7) != 0;
        self.ns.regs.set_flag(FlagType::C, c);
        self.set_reg8(r, new_val);
    }

    fn rr(&mut self, r: Reg8) {
        let val = self.get_reg8(r);
        let new_val = val >> 1 | (self.cpu.regs.get_flag(FlagType::C) as u8) << 7;

        self.ns.regs.set_flag(FlagType::Z, new_val == 0);
        self.ns.regs.set_flag(FlagType::N, false);
        self.ns.regs.set_flag(FlagType::H, false);
        self.ns.regs.set_flag(FlagType::C, val & 0x1 == 0);
        self.set_reg8(r, new_val);
    }

    fn alu(&mut self, op: u8, other_val: u8) {
        let a_val = self.get_reg8(Reg8::A);
        match op {
            0 => {
                // ADD A, r[z]
                let (new_a_val, overflow) = a_val.overflowing_add(other_val);
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, false);

                let hc = new_a_val & 0xf < a_val & 0xf;
                self.ns.regs.set_flag(FlagType::H, hc);
                self.ns.regs.set_flag(FlagType::C, overflow);
            }
            1 => {
                // ADC A,
                let (val1, o1) = a_val.overflowing_add(other_val);
                let (new_a_val, o2) =
                    val1.overflowing_add(self.cpu.regs.get_flag(FlagType::C) as u8);
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, false);

                let hc = new_a_val & 0xf < a_val & 0xf;
                self.ns.regs.set_flag(FlagType::H, hc);
                self.ns.regs.set_flag(FlagType::C, o1 || o2);
            }
            2 => {
                // SUB
                let (new_a_val, overflow) = a_val.overflowing_sub(other_val);
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, true);

                let hc = a_val & 0xf < other_val & 0xf;
                self.ns.regs.set_flag(FlagType::H, hc);
                self.ns.regs.set_flag(FlagType::C, overflow);
            }
            3 => {
                // SBC A,
                let with_carry = a_val as u16 | (self.cpu.regs.get_flag(FlagType::C) as u16) << 8;
                let (new_a_val, overflow) = with_carry.overflowing_sub(other_val as u16);
                self.set_reg8(Reg8::A, new_a_val as u8);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, true);

                let hc = with_carry & 0xff < other_val as u16 & 0xff;
                self.ns.regs.set_flag(FlagType::H, hc);
                self.ns.regs.set_flag(FlagType::C, overflow);
            }
            4 => {
                // AND
                let new_a_val = a_val & other_val;
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, false);
                self.ns.regs.set_flag(FlagType::H, true);
                self.ns.regs.set_flag(FlagType::C, false);
            }
            5 => {
                // XOR
                let new_a_val = a_val ^ other_val;
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, false);
                self.ns.regs.set_flag(FlagType::H, false);
                self.ns.regs.set_flag(FlagType::C, false);
            }
            6 => {
                // OR
                let new_a_val = a_val | other_val;
                self.set_reg8(Reg8::A, new_a_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, false);
                self.ns.regs.set_flag(FlagType::H, false);
                self.ns.regs.set_flag(FlagType::C, false);
            }
            7 => {
                // CP
                let (new_a_val, overflow) = a_val.overflowing_sub(other_val);
                self.ns.regs.set_flag(FlagType::Z, new_a_val == 0);
                self.ns.regs.set_flag(FlagType::N, true);

                let hc = a_val & 0xf < other_val & 0xf;
                self.ns.regs.set_flag(FlagType::H, hc);
                self.ns.regs.set_flag(FlagType::C, overflow);
            }
            _ => panic!("Impossible"),
        }
    }

    fn generate_interrupt(&mut self, intr: Interrupt) {
        self.ns.cycles = 4;
        let cur_pc = self.cpu.regs.pc;
        self.call(intr as u16, cur_pc);
    }

    fn generate(&mut self) {
        // TODO: Check flags.
        // TODO: handle overflow overflowing_add
        // Remove extra {} when only match expression.

        self.ns.regs = self.cpu.regs;

        // Uninitialized to ensure every instruction defines these.
        let cycles: u8;
        let size: u16;

        let byte0 = self.read_pc_val(0);
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
                                size = 1;
                                cycles = 4;
                            }
                            1 => {
                                // LD (a16),SP
                                size = 3;
                                cycles = 20;

                                let addr = self.read_pc_val16(1);
                                let val = self.cpu.regs.sp;
                                self.ns.mem_writes.push((addr, val as u8));
                                self.ns.mem_writes.push((addr + 1, (val >> 8) as u8));
                            }
                            2 => {
                                // STOP
                                size = 2;
                                cycles = 4;
                                self.ns.regs.stopped = true;
                            }
                            3...7 => {
                                // y == 3: JR d
                                // y > 3: JR cc[y-4], d
                                size = 2;

                                if y == 3 || self.cpu.regs.flag_idx_pass(y - 4) {
                                    cycles = 12;
                                    let val = self.read_pc_val(1) as i8;
                                    let sign_pc = self.cpu.regs.get16(Reg16::PC) as i32;

                                    // Add 2 to skip the current instruction.
                                    let next_pc = sign_pc + 2 + val as i32;
                                    self.ns.regs.set16(Reg16::PC, next_pc as u16);
                                } else {
                                    cycles = 8;
                                }
                            }
                            _ => panic!("Impossible"),
                        }
                    }
                    1 => {
                        if q == 0 {
                            // LD rp[p], nn
                            size = 3;
                            cycles = 12;

                            let val = self.read_pc_val16(1);
                            let dst_reg = rp_idx_to_r16(p);
                            self.ns.regs.set16(dst_reg, val);
                        } else {
                            size = 1;
                            cycles = 8;
                            // ADD HL, rp[p]
                            let add_val = self.cpu.regs.get16(rp_idx_to_r16(p));
                            self.ns.regs.hl += add_val;
                            self.ns.regs.set_flag(FlagType::N, false);
                        }
                    }
                    2 => {
                        if q == 0 {
                            match p {
                                0 | 1 => {
                                    size = 1;
                                    cycles = 8;

                                    let dst_addr = if p == 0 {
                                        // LD (BC), A
                                        self.cpu.regs.bc
                                    } else {
                                        // LD (DE), A
                                        self.cpu.regs.de
                                    };

                                    let write_op = (dst_addr, self.get_reg8(Reg8::A));
                                    self.ns.mem_writes.push(write_op);
                                }
                                2 | 3 => {
                                    size = 1;
                                    cycles = 8;

                                    let write_op = (self.cpu.regs.hl, self.get_reg8(Reg8::A));
                                    self.ns.mem_writes.push(write_op);

                                    if p == 2 {
                                        // LD (HL+),A
                                        self.ns.regs.hl += 1;
                                    } else {
                                        // LD (HL-),A
                                        self.ns.regs.hl -= 1;
                                    }
                                }
                                _ => panic!("Impossible"),
                            }
                        } else {
                            match p {
                                0 | 1 => {
                                    size = 1;
                                    cycles = 8;

                                    let addr = if p == 0 {
                                        // LD A, (BC)
                                        self.cpu.regs.bc
                                    } else {
                                        //  LD A, (DE)
                                        self.cpu.regs.de
                                    };
                                    let val = self.mem.read(addr);
                                    self.set_reg8(Reg8::A, val);
                                }
                                2 | 3 => {
                                    size = 1;
                                    cycles = 8;

                                    let val = self.mem.read(self.cpu.regs.hl);
                                    self.set_reg8(Reg8::A, val);

                                    if p == 2 {
                                        // LD A,(HL+)
                                        self.ns.regs.hl += 1;
                                    } else {
                                        // LD A,(HL-)
                                        self.ns.regs.hl -= 1;
                                    }
                                }
                                _ => panic!("Impossible"),
                            }
                        }
                    }
                    3 => {
                        size = 1;
                        cycles = 8;
                        let reg = rp_idx_to_r16(p);
                        let val = self.cpu.regs.get16(reg);
                        let new_val = if q == 0 {
                            // INC rp[p]
                            val.overflowing_add(1).0
                        } else {
                            // DEC rp[p]
                            val.overflowing_sub(1).0
                        };
                        self.ns.regs.set16(reg, new_val);
                    }
                    4 | 5 => {
                        size = 1;

                        let reg = r_idx_to_r8(y);
                        cycles = if reg == Reg8::HLI { 12 } else { 4 };

                        let val = self.get_reg8(reg);
                        let new_val;
                        if z == 4 {
                            // INC r[y]
                            new_val = val.overflowing_add(1).0;
                            self.ns.regs.set_flag(FlagType::N, false);
                        } else {
                            // DEC r[y]
                            new_val = val.overflowing_sub(1).0;
                            self.ns.regs.set_flag(FlagType::N, true);
                        }

                        self.set_reg8(reg, new_val);
                        self.ns.regs.set_flag(FlagType::Z, new_val == 0);

                        // Half carry occured if 4th bit changed.
                        let hc = val & !(1 << 4) != new_val & !(1 << 4);
                        self.ns.regs.set_flag(FlagType::H, hc);
                    }
                    6 => {
                        // LD r[y], n
                        size = 2;
                        let val = self.read_pc_val(1);
                        let reg = r_idx_to_r8(y);
                        cycles = if reg == Reg8::HLI { 12 } else { 8 };
                        self.set_reg8(reg, val);
                    }
                    7 => {
                        size = 1;
                        cycles = 4;
                        match y {
                            0 => {
                                // RLCA
                                self.rlc(Reg8::A);
                            }
                            1 => {
                                // RRCA
                                self.rrc(Reg8::A);
                            }
                            2 => {
                                // RLA
                                self.rl(Reg8::A);
                            }
                            3 => {
                                // RRA
                                self.rr(Reg8::A);
                            }
                            4 => {
                                // DAA
                                let mut val = self.get_reg8(Reg8::A) as u16;
                                let mut bcd: u16 = 0;
                                let mut iters = 0;
                                while val > 0 {
                                    bcd |= (val % 10) & 0xf << iters;

                                    iters += 1;
                                    val /= 10;
                                }

                                let new_val = bcd as u8;
                                self.ns.regs.set_flag(FlagType::Z, new_val == 0);
                                self.ns.regs.set_flag(FlagType::H, false);
                                self.ns.regs.set_flag(FlagType::C, bcd > 0xff);
                                self.set_reg8(Reg8::A, new_val);
                            }
                            5 => {
                                // CPL
                                let val = self.get_reg8(Reg8::A);
                                let new_val = !val;
                                self.ns.regs.set_flag(FlagType::N, true);
                                self.ns.regs.set_flag(FlagType::H, true);
                                self.set_reg8(Reg8::A, new_val);
                            }
                            6 => {
                                // SCF
                                self.ns.regs.set_flag(FlagType::N, false);
                                self.ns.regs.set_flag(FlagType::H, false);
                                self.ns.regs.set_flag(FlagType::C, true);
                            }
                            7 => {
                                // CCF
                                self.ns.regs.set_flag(FlagType::N, false);
                                self.ns.regs.set_flag(FlagType::H, false);

                                let c = self.ns.regs.get_flag(FlagType::C);
                                self.ns.regs.set_flag(FlagType::C, !c);
                            }
                            _ => panic!("Impossible"),
                        }
                    }
                    _ => panic!("Impossible"),
                }
            }
            1 => {
                if z == 6 && y == 6 {
                    // HALT
                    size = 1;
                    cycles = 4;
                    self.ns.regs.halted = true;
                } else {
                    // LD r[y], r[z]
                    size = 1;
                    let src = r_idx_to_r8(z);
                    let dst = r_idx_to_r8(y);

                    cycles = if src == Reg8::HLI || dst == Reg8::HLI {
                        8
                    } else {
                        4
                    };

                    let src_val = self.get_reg8(src);
                    self.set_reg8(dst, src_val);
                }
            }
            2 => {
                size = 1;
                let other = r_idx_to_r8(z);
                cycles = if other == Reg8::HLI { 8 } else { 4 };
                let other_val = self.get_reg8(other);
                self.alu(y, other_val);
            }
            3 => {
                match z {
                    0 => {
                        match y {
                            0...3 => {
                                // RET cc[y]
                                size = 1;
                                if self.cpu.regs.flag_idx_pass(y) {
                                    cycles = 20;
                                } else {
                                    cycles = 8;
                                }
                                self.ret();
                            }
                            4 => {
                                // LDH (a8),A
                                size = 2;
                                cycles = 12;

                                let offset = self.read_pc_val(1) as u16;
                                let addr = 0xff00 + offset;
                                let aval = self.get_reg8(Reg8::A);
                                self.ns.mem_writes.push((addr, aval));
                            }
                            5 | 7 => {
                                size = 2;
                                let to_add: u16 = self.read_pc_val(1) as u16;
                                let (new_sp, overflow) = self.cpu.regs.sp.overflowing_add(to_add);

                                if y == 5 {
                                    // ADD SP,n
                                    self.ns.regs.sp = new_sp;
                                    cycles = 16;
                                } else {
                                    // LD HL,SP+r8
                                    cycles = 12;
                                    self.ns.regs.hl = new_sp;
                                }

                                self.ns.regs.set_flag(FlagType::Z, false);
                                self.ns.regs.set_flag(FlagType::N, false);

                                // Set half carry if the 12th bit changed.
                                let hc = self.cpu.regs.sp & (1 << 12) != new_sp & (1 << 12);
                                self.ns.regs.set_flag(FlagType::H, hc);
                                self.ns.regs.set_flag(FlagType::C, overflow);
                            }
                            6 => {
                                // LDH A,(a8)
                                size = 2;
                                cycles = 12;

                                let offset = self.read_pc_val(1) as u16;
                                let addr = 0xff00 + offset;
                                let val = self.mem.read(addr);
                                self.set_reg8(Reg8::A, val);
                            }
                            _ => panic!("Impossible"),
                        }
                    }
                    1 => {
                        if q == 0 {
                            // OP rp2[p]
                            size = 1;
                            cycles = 12;

                            let reg = rp2_idx_to_r16(p);
                            let val = self.pop();
                            self.ns.regs.set16(reg, val);
                        } else {
                            match p {
                                0 => {
                                    // RET
                                    size = 1;
                                    cycles = 8;
                                    self.ret();
                                }
                                1 => {
                                    // RETI
                                    size = 1;
                                    cycles = 8;
                                    self.ns.regs.enable_interrupts = true;
                                    self.ret();
                                }
                                2 => {
                                    // JP (HL)
                                    size = 1;
                                    cycles = 4;
                                    self.ns.regs.pc = self.cpu.regs.hl;
                                }
                                3 => {
                                    // LD SP, HL
                                    size = 1;
                                    cycles = 8;
                                    self.ns.regs.sp = self.cpu.regs.hl;
                                }
                                _ => panic!("Impossible"),
                            }
                        }
                    }
                    2 => {
                        match y {
                            0...3 => {
                                size = 3;
                                // JP cc[y], nn
                                if self.cpu.regs.flag_idx_pass(y) {
                                    cycles = 16;
                                    self.ns.regs.pc = self.read_pc_val(1) as u16 |
                                                      (self.read_pc_val(2) as u16) << 8;
                                } else {
                                    cycles = 12;
                                }
                            }
                            4 => {
                                // LD (C),A
                                size = 1;
                                cycles = 8;

                                let addr = 0xff00 + self.get_reg8(Reg8::C) as u16;
                                let aval = self.get_reg8(Reg8::A);
                                self.ns.mem_writes.push((addr, aval));
                            }
                            5 => {
                                // LD (a16),A
                                size = 3;
                                cycles = 16;

                                let addr = self.read_pc_val16(1);
                                let aval = self.get_reg8(Reg8::A);
                                self.ns.mem_writes.push((addr, aval));
                            }
                            6 => {
                                // LD A,(C)
                                size = 1;
                                cycles = 8;

                                let addr = 0xff00 + self.get_reg8(Reg8::C) as u16;
                                let mem_val = self.mem.read(addr);
                                self.set_reg8(Reg8::A, mem_val);
                            }
                            7 => {
                                // LD A,(a16)
                                size = 3;
                                cycles = 16;

                                let addr = self.read_pc_val16(1);
                                let mem_val = self.mem.read(addr);
                                self.set_reg8(Reg8::A, mem_val);
                            }
                            _ => panic!("Impossible"),
                        }
                    }
                    3 => {
                        match y {
                            0 => {
                                // JP nn
                                size = 3;
                                cycles = 12;

                                self.ns.regs.pc = self.read_pc_val16(1);
                            }
                            1 => {
                                // CB prefix
                                let inst = self.read_pc_val(1);
                                let x = (inst >> 6) & 0x3; // [7:6]
                                let y = (inst >> 3) & 0x7; // [5:3]
                                let z = (inst >> 0) & 0x7; // [2:0]

                                match x {
                                    0 => {
                                        size = 2;
                                        let reg = r_idx_to_r8(z);
                                        cycles = if reg == Reg8::HLI { 16 } else { 8 };

                                        match y {
                                            0 => {
                                                // RLC
                                                self.rlc(reg);
                                            }
                                            1 => {
                                                // RRC
                                                self.rrc(reg);
                                            }
                                            2 => {
                                                // RL
                                                self.rl(reg);
                                            }
                                            3 => {
                                                // RR
                                                self.rr(reg);
                                            }
                                            4 => {
                                                // SLA
                                                let val = self.get_reg8(reg);
                                                let new_val = val << 1;
                                                self.set_reg8(reg, new_val);
                                                self.ns.regs.set_flag(FlagType::Z, new_val == 0);
                                                self.ns.regs.set_flag(FlagType::N, false);
                                                self.ns.regs.set_flag(FlagType::H, false);
                                                self.ns
                                                    .regs
                                                    .set_flag(FlagType::C, new_val & (1 << 7) != 0);
                                            }
                                            5 | 7 => {
                                                // 5. SRA
                                                // 7. SRL
                                                let val = self.get_reg8(reg);
                                                let mut new_val = val >> 1;
                                                if y == 5 {
                                                    new_val |= 0x80;
                                                }
                                                self.set_reg8(reg, new_val);
                                                self.ns.regs.set_flag(FlagType::Z, new_val == 0);
                                                self.ns.regs.set_flag(FlagType::N, false);
                                                self.ns.regs.set_flag(FlagType::H, false);
                                                self.ns
                                                    .regs
                                                    .set_flag(FlagType::C, new_val & (1 << 7) != 0);
                                            }
                                            6 => {
                                                // SWAP
                                                let val = self.get_reg8(reg);
                                                let new_val = val >> 4 | val << 4;
                                                self.set_reg8(reg, new_val);
                                                self.ns.regs.set_flag(FlagType::Z, new_val == 0);
                                                self.ns.regs.set_flag(FlagType::N, false);
                                                self.ns.regs.set_flag(FlagType::H, false);
                                                self.ns.regs.set_flag(FlagType::C, false);
                                            }
                                            _ => panic!("Impossible"),
                                        }
                                    }
                                    1 => {
                                        // BIT y, r[z]
                                        size = 2;
                                        let reg = r_idx_to_r8(z);
                                        cycles = if reg == Reg8::HLI { 16 } else { 8 };

                                        let val = self.get_reg8(reg);
                                        let z = val & (1 << y) == 0;
                                        self.ns.regs.set_flag(FlagType::Z, z);
                                        self.ns.regs.set_flag(FlagType::N, false);
                                        self.ns.regs.set_flag(FlagType::H, true);

                                    }
                                    2 => {
                                        // RES y, r[z]
                                        size = 2;
                                        let reg = r_idx_to_r8(z);
                                        cycles = if reg == Reg8::HLI { 16 } else { 8 };
                                        let val = self.get_reg8(reg);
                                        self.set_reg8(reg, val & !(1 << y));
                                    }
                                    3 => {
                                        // SET y, r[z]
                                        size = 2;
                                        let reg = r_idx_to_r8(z);
                                        cycles = if reg == Reg8::HLI { 16 } else { 8 };
                                        let val = self.get_reg8(reg);
                                        self.set_reg8(reg, val | (1 << y));
                                    }
                                    _ => panic!("Impossible"),
                                }
                            }
                            2 | 3 => {
                                // Removed instructions
                                // 2: OUT (n), A
                                // 3: IN A, (n)
                                // 4: EX (SP), HL
                                // 5: EX DE, HL
                                //
                                size = 1;
                                cycles = 4;
                            }
                            6 => {
                                // DI
                                size = 1;
                                cycles = 4;
                                self.ns.regs.enable_interrupts = false;
                            }
                            7 => {
                                // EI
                                size = 1;
                                cycles = 4;
                                self.ns.regs.enable_interrupts = true;
                            }
                            _ => panic!("Impossible"),
                        }
                    }
                    4 => {
                        // CALL cc[y], nn
                        size = 3;
                        if self.cpu.regs.flag_idx_pass(y) {
                            cycles = 24;
                            let addr = self.read_pc_val16(1);
                            let ret_addr = self.cpu.regs.pc + 3;
                            self.call(addr, ret_addr);
                        } else {
                            cycles = 12;
                        }
                    }
                    5 => {
                        if q == 0 {
                            // PUSH rp2[p]
                            size = 1;
                            cycles = 16;
                            let val = self.cpu.regs.get16(rp2_idx_to_r16(p));
                            self.push(val);
                        } else {
                            if p == 0 {
                                // CALL nn
                                size = 3;
                                cycles = 12;

                                let addr = self.read_pc_val16(1);
                                let ret_addr = self.cpu.regs.pc + 3;
                                self.call(addr, ret_addr);
                            } else {
                                // 1. DD prefix
                                // 2. ED prefix
                                // 3. FD prefix
                                size = 1;
                                cycles = 4;
                            }
                        }
                    }
                    6 => {
                        size = 2;
                        cycles = 8;

                        let other_val = self.read_pc_val(1);
                        self.alu(y, other_val);
                    }
                    7 => {
                        // RST
                        size = 1;
                        cycles = 32;

                        let pc = self.cpu.regs.pc;
                        self.push(pc);
                        self.ns.regs.pc = y as u16 * 8;
                    }
                    _ => panic!("Impossible"),
                }
            }
            _ => panic!("Impossible"),
        }

        self.ns.cycles = cycles;

        // Increment PC to next instruction if it didn't change.
        if self.ns.regs.pc == self.cpu.regs.pc {
            self.ns.regs.pc += size;
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

use std::fmt;
use std::io::{self, Read};

use cpu;

// TODO: Enable speed switch for gbc

const HIGH_RAM_BASE: u16 = 0xfe00;

#[derive(Copy, Clone, PartialEq)]
enum MbcType {
    MbcNone,
    Mbc1,
    Mbc2,
    Mbc3,
}

// TODO: Support other cartridge types. Only suppors mbc1 now.
struct Cartridge {
    // 0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
    // 4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
    rom: Vec<[u8; 0x4000]>,

    // A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
    ext_ram: Vec<[u8; 0x2000]>,

    mbc_type: MbcType,
    cur_rom_bank: u8,
    cur_rom_or_ram_bank: u8,
    ext_ram_enabled: bool,
    rom_bank_mode: bool,
}

pub enum RegAddr {
    P1 = 0xff00,
    SB = 0xff01,
    SC = 0xff02,
    DIV = 0xff04,
    TIMA = 0xff05,
    TMA = 0xff06,
    TAC = 0xff07,
    IF = 0xff0f,
    NR10 = 0xff10,
    NR11 = 0xff11,
    NR12 = 0xff12,
    NR14 = 0xff14,
    NR21 = 0xff16,
    NR22 = 0xff17,
    NR24 = 0xff19,
    NR30 = 0xff1a,
    NR31 = 0xff1b,
    NR32 = 0xff1c,
    NR33 = 0xff1e,
    NR41 = 0xff20,
    NR42 = 0xff21,
    NR43 = 0xff22,
    NR44 = 0xff23,
    NR50 = 0xff24,
    NR51 = 0xff25,
    NR52 = 0xff26,
    LCDC = 0xff40,
    SCY = 0xff42,
    SCX = 0xff43,
    LY = 0xff44,
    LYC = 0xff45,
    DMA = 0xff46,
    BGP = 0xff47,
    OBP0 = 0xff48,
    OBP1 = 0xff49,
    WY = 0xff4a,
    WX = 0xff4b,
    SVBK = 0xff70,
    IE = 0xffff,
}

pub struct Mem {
    cartridge: Cartridge,

    // 8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
    vram: [[u8; 0x4000]; 2],

    // C000-CFFF   4KB Work RAM Bank 0 (WRAM)
    // D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
    work_ram: [[u8; 0x1000]; 8],

    // E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
    //
    // FE00-FE9F   Sprite Attribute Table (OAM)
    // FEA0-FEFF   Not Usable
    // FF00-FF7F   I/O Ports
    // FF80-FFFE   High RAM (HRAM)
    // FFFF        Interrupt Enable Register
    high_ram: [u8; 0x200],
}

#[derive(Debug)]
pub enum CartErr {
    Io(io::Error),
    Length(usize, String),
    Invalid(String),
}

impl fmt::Display for CartErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CartErr::Io(ref e) => write!(f, "IO error: {}", e),
            &CartErr::Length(ref size, ref msg) => {
                write!(f, "Length error: {}. Size: {}", msg, size)
            }
            &CartErr::Invalid(ref msg) => write!(f, "Invalid input: {}", msg),
        }
    }
}

impl Default for Cartridge {
    fn default() -> Cartridge {
        Cartridge {
            rom: vec![],
            ext_ram: vec![],
            mbc_type: MbcType::MbcNone,
            cur_rom_bank: 0,
            cur_rom_or_ram_bank: 0,
            ext_ram_enabled: false,
            rom_bank_mode: true,
        }
    }
}

impl Default for Mem {
    fn default() -> Mem {
        let mut val = Mem {
            cartridge: Default::default(),
            vram: [[0; 0x4000]; 2],
            work_ram: [[0; 0x1000]; 8],
            high_ram: [0; 0x200],
        };

        val.high_ram[RegAddr::TIMA as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::TMA as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::TAC as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::NR10 as usize - HIGH_RAM_BASE as usize] = 0x80;
        val.high_ram[RegAddr::NR11 as usize - HIGH_RAM_BASE as usize] = 0xbf;
        val.high_ram[RegAddr::NR12 as usize - HIGH_RAM_BASE as usize] = 0xf3;
        val.high_ram[RegAddr::NR14 as usize - HIGH_RAM_BASE as usize] = 0xbf;
        val.high_ram[RegAddr::NR21 as usize - HIGH_RAM_BASE as usize] = 0x3f;
        val.high_ram[RegAddr::NR22 as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::NR24 as usize - HIGH_RAM_BASE as usize] = 0xbf;
        val.high_ram[RegAddr::NR30 as usize - HIGH_RAM_BASE as usize] = 0x7f;
        val.high_ram[RegAddr::NR31 as usize - HIGH_RAM_BASE as usize] = 0xff;
        val.high_ram[RegAddr::NR32 as usize - HIGH_RAM_BASE as usize] = 0x9f;
        val.high_ram[RegAddr::NR33 as usize - HIGH_RAM_BASE as usize] = 0xbf;
        val.high_ram[RegAddr::NR41 as usize - HIGH_RAM_BASE as usize] = 0xff;
        val.high_ram[RegAddr::NR42 as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::NR43 as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::NR44 as usize - HIGH_RAM_BASE as usize] = 0xbf;
        val.high_ram[RegAddr::NR50 as usize - HIGH_RAM_BASE as usize] = 0x77;
        val.high_ram[RegAddr::NR51 as usize - HIGH_RAM_BASE as usize] = 0xf3;
        val.high_ram[RegAddr::NR52 as usize - HIGH_RAM_BASE as usize] = 0xf1;
        val.high_ram[RegAddr::LCDC as usize - HIGH_RAM_BASE as usize] = 0x91;
        val.high_ram[RegAddr::SCY as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::SCX as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::LY as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::LYC as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::BGP as usize - HIGH_RAM_BASE as usize] = 0xfc;
        val.high_ram[RegAddr::OBP0 as usize - HIGH_RAM_BASE as usize] = 0xff;
        val.high_ram[RegAddr::OBP1 as usize - HIGH_RAM_BASE as usize] = 0xff;
        val.high_ram[RegAddr::WY as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::WX as usize - HIGH_RAM_BASE as usize] = 0x00;
        val.high_ram[RegAddr::IE as usize - HIGH_RAM_BASE as usize] = 0x00;

        val
    }
}

fn read_bank(rom: &mut Read) -> Result<[u8; 0x4000], CartErr> {
    let mut buf = [0u8; 0x4000];
    let len = rom.read(&mut buf).map_err(CartErr::Io)?;
    if len != buf.len() {
        return Err(CartErr::Length(len, "Short read".to_string()));
    }

    Ok(buf)
}

impl Cartridge {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x3fff => self.rom[0][addr as usize],
            0x4000...0x7fff => {
                match self.mbc_type {
                    MbcType::MbcNone => self.rom[1][addr as usize - 0x4000],
                    MbcType::Mbc1 => {
                        let base_idx = if self.rom_bank_mode {
                            self.cur_rom_or_ram_bank << 5 | self.cur_rom_bank
                        } else {
                            self.cur_rom_bank
                        };

                        let real_idx = match base_idx {
                            0 => 1,
                            0x20 | 0x40 | 0x60 => base_idx + 1,
                            _ => base_idx,
                        };

                        self.rom[real_idx as usize][addr as usize - 0x4000]
                    }
                    _ => self.rom[self.cur_rom_bank as usize][addr as usize - 0x4000],
                }
            }
            0xa000...0xbfff => {
                let bank_num = if self.rom_bank_mode {
                    0
                } else {
                    self.cur_rom_or_ram_bank
                } as usize;
                if bank_num >= self.ext_ram.len() {
                    return 0xff;
                }
                self.ext_ram[bank_num][addr as usize - 0xa000]
            }
            _ => panic!("Invalid read to cartridge"),
        }
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000...0x1fff => {
                self.ext_ram_enabled = val & 0xf == 0xa;
            }
            0x2000...0x3fff => {
                let mask = 0x1fu8;
                self.cur_rom_bank = self.cur_rom_bank & !mask | val & mask;
            }
            0x4000...0x5fff => {
                self.cur_rom_or_ram_bank = val & 0x3;
            }
            0x6000...0x7fff => {
                self.rom_bank_mode = val & 0x1 == 0;
            }
            0xa000...0xbfff => {
                let bank_num = if self.rom_bank_mode {
                    0
                } else {
                    self.cur_rom_or_ram_bank
                } as usize;
                if bank_num >= self.ext_ram.len() {
                    return;
                }
                self.ext_ram[bank_num][addr as usize - 0xa000] = val;
            }
            _ => panic!("Invalid write to cartridge"),
        }
    }

    pub fn load(&mut self, rom: &mut Read) -> Result<(), CartErr> {
        let bank0 = read_bank(rom)?;

        // TODO: Handle 0143 - CGB Flag

        let cart_type = bank0[0x147];
        match cart_type {
            // TODO: Handle other cartridge types.
            0x01...0x03 => self.mbc_type = MbcType::Mbc1,
            0x05...0x06 => self.mbc_type = MbcType::Mbc2,
            0x0f...0x13 => self.mbc_type = MbcType::Mbc3,
            _ => {
                return Err(CartErr::Invalid(format!("Unsupported cartridge type {:x}", cart_type)))
            }
        }

        let rom_size = bank0[0x148];
        let normal = 2 << rom_size;
        let num_rom_banks: usize = match rom_size {
            0x00...0x04 | 7 => normal,
            0x05 | 0x06 => {
                if self.mbc_type == MbcType::Mbc1 {
                    normal - 1
                } else {
                    normal
                }
            }
            0x52 => 72,
            0x53 => 80,
            0x57 => 96,
            _ => return Err(CartErr::Invalid(format!("Invalid ROM size {:x}", rom_size))),
        };

        let ram_size = bank0[0x149];
        let num_ram_banks: usize = match ram_size {
            0x00 => 0,
            0x01 | 0x2 => 1,
            0x03 => 4,
            _ => return Err(CartErr::Invalid(format!("Invalid RAM size {:x}", ram_size))),
        };

        // Handle ROM allocation
        // TODO: Check rom layout allocation. e.g. mbc1 can't handle banks 20,40,60
        self.rom.reserve(num_rom_banks);
        self.rom.push(bank0);
        for _ in 1..num_rom_banks {
            let buf = read_bank(rom)?;
            self.rom.push(buf);
        }

        // Handle RAM allocation
        let zeros = [0u8; 0x2000];
        self.ext_ram.reserve(num_ram_banks);
        for _ in 0..num_ram_banks {
            self.ext_ram.push(zeros);
        }

        Ok(())
    }
}

impl Mem {
    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x7fff | 0xa000...0xbfff => self.cartridge.read(addr),
            0x8000...0x9fff => {
                // FF4F - VBK - CGB Mode Only - VRAM Bank
                let vbk = self.read(0xff4f);
                if vbk & 0x1 == 0 {
                    self.vram[0][addr as usize - 0x8000]
                } else {
                    self.vram[1][addr as usize - 0x8000]
                }
            }
            0xc000...0xcfff => self.work_ram[0][addr as usize - 0xc000],
            0xd000...0xdfff => {
                // FF70 - SVBK - CGB Mode Only - WRAM Bank
                let svbk = self.read_reg(RegAddr::SVBK);
                let masked = svbk & 0x7;
                let idx = if masked == 0 { 1 } else { masked };
                self.work_ram[idx as usize][addr as usize - 0xd000]
            }
            0xe000...0xfdff => self.read(addr - 0x2000),
            0xfe00...0xffff => self.high_ram[addr as usize - 0xfe00],
            _ => panic!("Impossible"),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000...0x7fff | 0xa000...0xbfff => self.cartridge.write(addr, val),
            0x8000...0x9fff => {
                // FF4F - VBK - CGB Mode Only - VRAM Bank
                let vbk = self.read(0xff4f);
                if vbk & 0x1 == 0 {
                    self.vram[0][addr as usize - 0x8000] = val;
                } else {
                    self.vram[1][addr as usize - 0x8000] = val;
                }
            }
            0xc000...0xcfff => self.work_ram[0][addr as usize - 0xc000] = val,
            0xd000...0xdfff => {
                // FF70 - SVBK - CGB Mode Only - WRAM Bank
                let svbk = self.read_reg(RegAddr::SVBK);
                let masked = svbk & 0x7;
                let idx = if masked == 0 { 1 } else { masked };
                self.work_ram[idx as usize][addr as usize - 0xd000] = val;
            }
            0xe000...0xfdff => self.write(addr - 0x2000, val),
            0xfe00...0xffff => {
                let mut write_val = val;
                // Handle special rules
                if addr == RegAddr::DIV as u16 {
                    write_val = 0;
                } else if addr == RegAddr::P1 as u16 {
                    let cur_val = self.read(RegAddr::P1 as u16);
                    let mask = 0x30;

                    write_val = val & mask | cur_val & !mask;
                } else if addr == RegAddr::SVBK as u16 {
                    write_val &= 0x7;
                }


                // TODO: testingremove
                if addr == RegAddr::SC as u16 {
                    let sb = self.read(RegAddr::SB as u16) as char;
                    println!("Console output: {}", sb);
                }

                self.high_ram[addr as usize - 0xfe00] = write_val;
            }
            _ => panic!("Impossible"),
        }
    }

    pub fn read_reg(&self, reg: RegAddr) -> u8 {
        self.high_ram[reg as usize - HIGH_RAM_BASE as usize]
    }

    pub fn write_reg(&mut self, reg: RegAddr, val: u8) {
        self.high_ram[reg as usize - HIGH_RAM_BASE as usize] = val;
    }

    pub fn set_interrupt_en(&mut self, intr: cpu::Interrupt, set: bool) {
        let mask = 1u8 << interrupt_bit(intr);
        let val = self.read_reg(RegAddr::IE);
        let new_val = if set { val | mask } else { val & !mask };
        self.write_reg(RegAddr::IE, new_val);
    }

    pub fn get_interrupt_en(&mut self, intr: cpu::Interrupt) -> bool {
        let mask = 1u8 << interrupt_bit(intr);
        let val = self.read_reg(RegAddr::IE);
        val & mask != 0
    }

    pub fn set_interrupt_flag(&mut self, intr: cpu::Interrupt, set: bool) {
        let mask = 1u8 << interrupt_bit(intr);
        let val = self.read_reg(RegAddr::IF);
        let new_val = if set { val | mask } else { val & !mask };
        self.write_reg(RegAddr::IF, new_val);
    }

    pub fn get_interrupt_flag(&mut self, intr: cpu::Interrupt) -> bool {
        let mask = 1u8 << interrupt_bit(intr);
        let val = self.read(RegAddr::IF as u16);
        val & mask != 0
    }

    pub fn load_cartridge(&mut self, rom: &mut Read) -> Result<(), CartErr> {
        self.cartridge.load(rom)
    }
}

fn interrupt_bit(intr: cpu::Interrupt) -> u8 {
    match intr {
        cpu::Interrupt::Vblank => 0,
        cpu::Interrupt::LcdStatus => 1,
        cpu::Interrupt::Timer => 2,
        cpu::Interrupt::Serial => 3,
        cpu::Interrupt::Joypad => 4,
    }
}

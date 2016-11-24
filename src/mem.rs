use std;

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
    cur_ram_bank: u8,
    cur_rom_or_ram_bank: u8,
    ext_ram_enabled: bool,
    rom_bank_mode: bool,
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
    oam: [u8; 160],

    // FEA0-FEFF   Not Usable
    //
    // FF00-FF7F   I/O Ports
    io_ports: [u8; 128],

    // FF80-FFFE   High RAM (HRAM)
    hi_ram: [u8; 127],

    // FFFF        Interrupt Enable Register
    inter_en: u8,
}

#[derive(Debug)]
pub enum CartErr {
    Io(std::io::Error),
    Length(usize, String),
    Invalid(String),
}

impl Default for Cartridge {
    fn default() -> Cartridge {
        Cartridge {
            rom: vec![],
            ext_ram: vec![],
            mbc_type: MbcType::MbcNone,
            cur_rom_bank: 0,
            cur_ram_bank: 0,
            cur_rom_or_ram_bank: 0,
            ext_ram_enabled: false,
            rom_bank_mode: true,
        }
    }
}

impl Default for Mem {
    fn default() -> Mem {
        Mem {
            cartridge: Default::default(),
            vram: [[0; 0x4000]; 2],
            work_ram: [[0; 0x1000]; 8],
            oam: [0; 160],
            io_ports: [0; 128],
            hi_ram: [0; 127],
            inter_en: 0,
        }
    }
}

fn read_bank(rom: &mut std::io::Read) -> Result<[u8; 0x4000], CartErr> {
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

                        self.rom[self.cur_rom_bank as usize][real_idx as usize - 0x4000]
                    }
                    _ => self.rom[self.cur_rom_bank as usize][addr as usize - 0x4000],
                }
            }
            0xa000...0xbfff => self.ext_ram[self.cur_ram_bank as usize][addr as usize - 0xa000],
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
                self.ext_ram[self.cur_ram_bank as usize][addr as usize - 0xa000] = val
            }
            _ => panic!("Invalid write to cartridge"),
        }
    }

    pub fn load(&mut self, rom: &mut std::io::Read) -> Result<(), CartErr> {
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
            0x0000...0x7fff | 0xa000...0xbfff => return self.cartridge.read(addr),
            _ => panic!("Unimplemented"),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000...0x7fff | 0xa000...0xbfff => return self.cartridge.write(addr, val),
            _ => panic!("Unimplemented"),
        }
    }

    pub fn load_cartridge(&mut self, rom: &mut std::io::Read) -> Result<(), CartErr> {
        self.cartridge.load(rom)
    }
}
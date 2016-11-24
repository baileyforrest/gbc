use cpu;
use mem;

enum TileMapSelect {
    A9800 = 0x9800,
    A9c00 = 0x9c00,
}

enum TileSelectData {
    A8800 = 0x8800,
    A8000 = 0x8000,
}

enum ObjSize {
    S8x8,
    S8x16,
}

#[derive(Copy, Clone, PartialEq)]
enum LcdMode {
    Hblank = 0,
    Vblank = 1,
    SearchOam = 2,
    LcdTransfer = 3,
}

pub struct Lcd {
    enabled: bool,
    window_tile_map_select: TileMapSelect,
    window_display_enable: bool,
    tile_select_data: TileSelectData,
    bg_tile_map_select: TileMapSelect,
    obj_size: ObjSize,
    obj_enable: bool,
    bg_display: bool,

    int_en_lyc_eq_ly: bool,
    int_en_oam: bool,
    int_en_vblank: bool,
    int_en_hblank: bool,

    lyc_eq_ly: bool, // TODO send interrupt
    mode: LcdMode,
    mode_cycles: usize,
    cur_row_num: u8,
}

impl Default for Lcd {
    fn default() -> Lcd {
        Lcd {
            enabled: false,
            window_tile_map_select: TileMapSelect::A9800,
            window_display_enable: false,
            tile_select_data: TileSelectData::A8800,
            bg_tile_map_select: TileMapSelect::A9800,
            obj_size: ObjSize::S8x8,
            obj_enable: false,
            bg_display: false,
            int_en_lyc_eq_ly: false,
            int_en_oam: false,
            int_en_vblank: false,
            int_en_hblank: false,
            lyc_eq_ly: false,
            mode: LcdMode::Vblank,
            mode_cycles: 0,
            cur_row_num: 0,
        }
    }
}

impl Lcd {
    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        self.process_control(mem);
        self.process_state(mem);
        self.process_status(mem);
    }

    // Check FF40 - LCDC - LCD Control (R/W)
    fn process_control(&mut self, mem: &mut mem::Mem) {
        let ctrl = mem.read(0xff40);

        // Bit 7 - LCD Display Enable             (0=Off, 1=On)
        if !self.enabled && ctrl & 0x80 != 0 {
            self.enabled = true;
        } else if self.enabled && ctrl & 0x80 == 0 {
            self.enabled = false;
        }

        // Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
        self.window_tile_map_select = if ctrl & 0x40 == 0 {
            TileMapSelect::A9800
        } else {
            TileMapSelect::A9c00
        };

        // Bit 5 - Window Display Enable          (0=Off, 1=On)
        self.window_display_enable = ctrl & 0x20 != 0;

        // Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
        self.tile_select_data = if ctrl & 0x10 == 0 {
            TileSelectData::A8800
        } else {
            TileSelectData::A8000
        };

        // Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
        self.bg_tile_map_select = if ctrl & 0x08 == 0 {
            TileMapSelect::A9800
        } else {
            TileMapSelect::A9c00
        };

        // Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
        self.obj_size = if ctrl & 0x04 == 0 {
            ObjSize::S8x8
        } else {
            ObjSize::S8x16
        };

        // Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
        self.obj_enable = ctrl & 0x02 != 0;

        // Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
        self.bg_display = ctrl & 0x01 != 0;
    }

    // Process LCD state
    fn process_state(&mut self, mem: &mut mem::Mem) {
        if self.mode_cycles != 0 {
            self.mode_cycles -= 1;
            return;
        }

        if self.mode == LcdMode::LcdTransfer {
            // FF44 - LY - LCDC Y-Coordinate (R)
            mem.write(0xff44, self.cur_row_num);

            // FF45 - LYC - LY Compare (R/W)
            let lyc = mem.read(0xff45);
            self.lyc_eq_ly = self.cur_row_num == lyc;

            if self.lyc_eq_ly && self.int_en_lyc_eq_ly {
                mem.set_interrupt_flag(cpu::Interrupt::LcdStatus, true);
            }
        }

        self.mode = match self.mode {
            LcdMode::SearchOam => LcdMode::LcdTransfer,
            LcdMode::LcdTransfer => LcdMode::Hblank,
            LcdMode::Hblank => {
                self.cur_row_num += 1;
                if self.cur_row_num == 154 {
                    self.cur_row_num = 0;
                    LcdMode::Vblank
                } else {
                    LcdMode::SearchOam
                }
            }
            LcdMode::Vblank => LcdMode::SearchOam,
        };

        self.mode_cycles = match self.mode {
            LcdMode::SearchOam => 80,
            LcdMode::LcdTransfer => 172,
            LcdMode::Hblank => 204,
            LcdMode::Vblank => 4560,
        };

        // Send state transition interrupts
        match self.mode {
            LcdMode::SearchOam => {
                if self.int_en_oam {
                    mem.set_interrupt_flag(cpu::Interrupt::LcdStatus, true);
                }
            }
            LcdMode::Hblank => {
                if self.int_en_hblank {
                    mem.set_interrupt_flag(cpu::Interrupt::LcdStatus, true);
                }
            }
            LcdMode::Vblank => {
                mem.set_interrupt_flag(cpu::Interrupt::Vblank, true);

                if self.int_en_vblank {
                    mem.set_interrupt_flag(cpu::Interrupt::LcdStatus, true);
                }
            }
            _ => (),
        }
    }

    // FF41 - STAT - LCDC Status (R/W)
    fn process_status(&mut self, mem: &mut mem::Mem) {
        const LCD_STATUS_ADDR: u16 = 0xff41;
        let status = mem.read(LCD_STATUS_ADDR);

        // Bit 6 - LYC=LY Coincidence Interrupt (1=Enable) (Read/Write)
        self.int_en_lyc_eq_ly = status & 0x20 != 0;

        // Bit 5 - Mode 2 OAM Interrupt         (1=Enable) (Read/Write)
        self.int_en_oam = status & 0x10 != 0;

        // Bit 4 - Mode 1 V-Blank Interrupt     (1=Enable) (Read/Write)
        self.int_en_vblank = status & 0x08 != 0;

        // Bit 3 - Mode 0 H-Blank Interrupt     (1=Enable) (Read/Write)
        self.int_en_hblank = status & 0x04 != 0;

        let new_status = status & !0x7u8 | (self.lyc_eq_ly as u8) << 2 | self.mode as u8;
        mem.write(LCD_STATUS_ADDR, new_status);
    }
}

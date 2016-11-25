use cpu;
use mem;

const SCREEN_WIDTH: u8 = 160;
const SCREEN_HEIGHT: u8 = 144;
const TILE_SIZE: u16 = 16;

pub type FrameBuffer = [[[u8; 3]; SCREEN_WIDTH as usize]; SCREEN_HEIGHT as usize];

#[derive(Copy, Clone)]
enum TileMapSelect {
    A9800 = 0x9800,
    A9c00 = 0x9c00,
}

#[derive(Copy, Clone)]
enum TileSelectData {
    A8800 = 0x8800,
    A8000 = 0x8000,
}

#[derive(Copy, Clone, PartialEq)]
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

    lyc_eq_ly: bool,
    mode: LcdMode,
    mode_cycles: usize,
    cur_row_num: u8,

    frame_buf: FrameBuffer,
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
            frame_buf: [[[0u8; 3]; SCREEN_WIDTH as usize]; SCREEN_HEIGHT as usize],
        }
    }
}

impl Lcd {
    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        self.process_control(mem);
        self.process_state(mem);
        self.process_status(mem);
    }

    pub fn get_frame_buffer(&self) -> &FrameBuffer {
        &self.frame_buf
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

            self.gen_line(mem);
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

    fn get_tile_addr(&self, idx: u8) -> u16 {
        match self.tile_select_data {
            TileSelectData::A8800 => TileSelectData::A8800 as u16 + idx as u16,
            TileSelectData::A8000 => {
                const BASE_ADDR: u16 = 0x9000;
                if idx & 0x80 == 0 {
                    BASE_ADDR + idx as u16
                } else {
                    let abs = !(idx as u16) + 1; // 2's complement
                    BASE_ADDR - abs
                }
            }
        }
    }

    fn gen_pixel_color(&self, mem: &mem::Mem, reg_addr: mem::RegAddr, color_idx: u8) -> [u8; 3] {
        let bgp = mem.read_reg(reg_addr);
        assert!(color_idx < 4);
        let shade = (bgp >> (color_idx * 2)) & 0x3;

        // TODO: Return RGB value
        // TODO: Support GBC colors
        [shade; 3]
    }

    fn gen_tile_color_idx(&self,
                          mem: &mem::Mem,
                          tile_base_addr: u16,
                          tile_x: u8,
                          tile_y: u8)
                          -> u8 {
        assert!(tile_x < 8 && tile_y < 8);

        let tile_byte_upper = mem.read(tile_base_addr + tile_y as u16 * 2);
        let tile_byte_lower = mem.read(tile_base_addr + tile_y as u16 * 2 + 1);

        let upper_bit = tile_byte_upper >> (7 - tile_x);
        let lower_bit = tile_byte_lower >> (7 - tile_x);
        let color_idx = upper_bit << 1 | lower_bit;

        color_idx
    }

    fn gen_color_idx(&self, mem: &mem::Mem, tile_map_addr: u16, map_x: u8, map_y: u8) -> u8 {
        let map_tile_x = (map_x + 7) / 8;
        let map_tile_y = (map_y + 7) / 8;

        let map_idx = map_tile_y as u16 * 32 + map_tile_x as u16;
        let map_addr = tile_map_addr + map_idx;
        let tile_idx = mem.read(map_addr);
        let tile_base_addr = self.get_tile_addr(tile_idx);

        let tile_x = map_x & 0x7;
        let tile_y = map_y & 0x7;

        self.gen_tile_color_idx(mem, tile_base_addr, tile_x, tile_y)
    }

    fn gen_line(&mut self, mem: &mem::Mem) {
        let mut color_idx_buf = [0u8; SCREEN_WIDTH as usize];
        self.frame_buf[self.cur_row_num as usize] = [[0u8; 3]; SCREEN_WIDTH as usize];
        let row = self.cur_row_num;

        // TODO: Handle different cases for gbc
        if self.bg_display {
            // Write background
            let scy = mem.read_reg(mem::RegAddr::SCY);
            let scx = mem.read_reg(mem::RegAddr::SCX);

            let tile_map_addr = self.bg_tile_map_select as u16;

            for col in 0..SCREEN_WIDTH {
                let bg_x = scx.overflowing_add(col).0;
                let bg_y = scy.overflowing_add(row).0;
                let color_idx = self.gen_color_idx(mem, tile_map_addr, bg_x, bg_y);
                color_idx_buf[col as usize] = color_idx;
                self.frame_buf[row as usize][col as usize] =
                    self.gen_pixel_color(mem, mem::RegAddr::BGP, color_idx);
            }
        }

        if self.window_display_enable {
            // Write window
            let wy = mem.read_reg(mem::RegAddr::WY);
            let wx = mem.read_reg(mem::RegAddr::WX);

            let tile_map_addr = self.window_tile_map_select as u16;

            for col in 0..SCREEN_WIDTH {
                if row < wy || col + 7 < wx {
                    // Do nothing if we're off the window
                    continue;
                }

                let win_x = col + 7 - wx;
                let win_y = row - wy;

                let color_idx = self.gen_color_idx(mem, tile_map_addr, win_x, win_y);
                color_idx_buf[col as usize] = color_idx;
                self.frame_buf[row as usize][col as usize] =
                    self.gen_pixel_color(mem, mem::RegAddr::BGP, color_idx);
            }
        }

        if self.obj_enable {
            // Write sprites
            const OAM_START: u16 = 0xfe00;
            const OAM_END: u16 = 0xfe00;
            const SPRITE_SIZE: u16 = 4;
            const TILE_MAP_ADDR: u16 = 0x8000;

            let mut cur_sprite = OAM_START;

            while cur_sprite <= OAM_END {
                let pos_y = mem.read(cur_sprite);
                let pos_x = mem.read(cur_sprite + 1);
                let mut tile_idx = mem.read(cur_sprite + 2) as u16;
                let flags = mem.read(cur_sprite + 3);

                if self.obj_size == ObjSize::S8x16 {
                    tile_idx &= 0xfe;
                }

                let tile_addr = TILE_MAP_ADDR + tile_idx * TILE_SIZE;

                for row in 0..16 {
                    for col in 0..8 {
                        if row >= 8 && self.obj_size == ObjSize::S8x8 {
                            continue;
                        }

                        // Do nothing if the sprite is off the screen
                        if pos_x >= SCREEN_WIDTH + 8 || pos_x + col < 8 ||
                           pos_y >= SCREEN_HEIGHT + 16 ||
                           pos_y + row < 16 {
                            continue;
                        }
                        let screen_x = pos_x - 8 + col;
                        let screen_y = pos_y - 8 + row;

                        // Only write on the current row
                        if screen_y != self.cur_row_num {
                            continue;
                        }

                        let above_bg = flags & 0x80 == 0;
                        let bg_idx = color_idx_buf[screen_x as usize];

                        // Do nothing if we are behind the background
                        if bg_idx != 0 && !above_bg {
                            continue;
                        }

                        let flip_x = flags & 0x20 != 0;
                        let flip_y = flags & 0x40 != 0;

                        // TODO: For 8x16, do the tile orders swap too?
                        let flipped_col = if flip_x { 7 - col } else { col };
                        let flipped_row = if flip_y { 7 - row } else { row };

                        let color_idx = if row >= 8 {
                            let lower_tile_addr = tile_addr + TILE_SIZE;
                            self.gen_tile_color_idx(mem, lower_tile_addr, flipped_col, flipped_row)
                        } else {
                            self.gen_tile_color_idx(mem, tile_addr, flipped_col, flipped_row)
                        };

                        let pallet_addr = if flags & 0x10 == 0 {
                            mem::RegAddr::OBP0
                        } else {
                            mem::RegAddr::OBP1
                        };

                        self.frame_buf[screen_y as usize][screen_x as usize] =
                            self.gen_pixel_color(mem, pallet_addr, color_idx);

                        // TODO Handle GBC Tile VRAM-Bank
                        // TODO Handle GBC Palette number
                    }
                }

                cur_sprite += SPRITE_SIZE;
            }
        }
    }
}

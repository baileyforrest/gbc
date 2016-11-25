use mem;

#[derive(Default)]
pub struct Dma {
}

impl Dma {
    pub fn on_clock(&mut self, mem: &mut mem::Mem) {
        self.oam_dma(mem);
    }

    pub fn oam_dma(&mut self, mem: &mut mem::Mem) {
        const DST_START: u16 = 0xfe00;
        const DST_END: u16 = 0xfe9f;

        let dma = mem.read_reg(mem::RegAddr::DMA);
        if dma == 0 {
            return;
        }
        mem.write_reg(mem::RegAddr::DMA, 0x00);

        // If the register was written to, just do the whole DMA right away.

        let mut dma_src = (dma as u16) << 8;
        let mut dma_dst = DST_START;

        while dma_dst <= DST_END {
            let val = mem.read(dma_src);
            mem.write(dma_dst, val);

            dma_src += 1;
            dma_dst += 1;
        }
    }
}

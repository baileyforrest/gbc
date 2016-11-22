#[derive(Default)]
pub struct Mem {
}

impl Mem {
    pub fn read(&self, addr: u16) -> u8 {
        // TODO
        let _ = addr;
        0x00
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        // TODO
        let _ = addr;
        let _ = val;
    }
}

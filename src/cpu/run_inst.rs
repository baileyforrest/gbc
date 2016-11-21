// Source: http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
//
// The following instructions have been removed:
// - Any command that uses the IX or IY registers.
// - All IN/OUT instructions.
// - All exchange instructions.
// - All commands prefixed by ED (except remapped RETI).
// - All conditional jumps/calls/rets on parity/overflow  and sign flag.
//
// The following instructions have different opcodes:
// - LD  A,[nnnn]
// - LD  [nnnn],A
// - RETI

// TODO: make sure special cases are handled.

use super::cpu;

#[derive(Copy, Clone)]
enum Prefix {
    CB,
    DD,
    FD,
}

impl cpu::Cpu {
    pub fn run_inst(&mut self) {
        let mut pc_offset: i16 = 1;

        // Check for instruction prefix.
        let prefix: Option<Prefix> = match self.read_pc_val(pc_offset - 1) {
            0xcb => Some(Prefix::CB),
            0xdd => Some(Prefix::DD),
            0xfd => Some(Prefix::FD),
            _ => {
                pc_offset = 0;
                None
            }  // No prefix, set to 0.
        };

    }
}

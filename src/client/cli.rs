extern crate libgbc;

use std::io::{self, Read, Write};
use std::io::BufRead;

#[derive(Default)]
pub struct Cli {
    gbc: libgbc::Gbc,
}

impl Cli {
    pub fn new() -> Cli {
        Default::default()
    }

    pub fn run(&mut self, rom: &mut Read) -> bool {
        if let Err(e) = self.gbc.load_rom(rom) {
            println!("Failed to load rom: {}", e);
            return false;
        }

        let mut line = String::new();
        let stdin = io::stdin();

        loop {
            print_prompt();
            match stdin.lock().read_line(&mut line) {
                Err(e) => {
                    println!("{}", e);
                    return false;
                }
                Ok(0) => {
                    println!("");
                    return true;
                }
                _ => self.run_command(&line),
            }
        }
    }

    fn run_command(&mut self, line: &String) {
        let _ = line;
    }
}

fn print_prompt() {
    print!("> ");
    let _ = io::stdout().flush();
}

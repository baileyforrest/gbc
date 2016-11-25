extern crate libgbc;

use std::collections::BTreeMap;
use std::io::{self, Read, Write};
use std::io::BufRead;

trait Cmd {
    fn help(&self) -> &'static str;
    fn name(&self) -> &'static str;
    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]);
}

#[derive(Default)]
pub struct Cli {
    gbc: libgbc::Gbc,
    last_cmd: String,
    cmds: BTreeMap<String, Box<Cmd>>,
}

impl Cli {
    pub fn new() -> Cli {
        let mut val: Cli = Default::default();
        val.register_cmds();
        val
    }

    fn register_cmds(&mut self) {
        let help = Box::new(CmdMem {});
        self.cmds.insert(help.name().to_string(), help);
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
            line.clear();
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

    fn run_command(&mut self, line: &str) {
        // Repeat last command if command is empty
        let cmd_str = if line == "\n" {
            self.last_cmd.clone()
        } else {
            line.to_string()
        };
        let cmd: Vec<&str> = cmd_str.split_whitespace().collect();
        if cmd.len() == 0 {
            return;
        }

        if cmd[0] == "help" {
            self.print_help();
            return;
        }

        match self.cmds.get_mut(cmd[0]) {
            None => {
                println!("Unknown command: {}. Try running `help`", cmd[0]);
                return;
            }
            Some(cmd_handler) => cmd_handler.run(&mut self.gbc, &cmd[1..]),
        }

        self.last_cmd = line.to_string();
    }

    fn print_help(&self) {
        println!("Commands:\n");
        for (name, cmd) in &self.cmds {
            print!("{} {}", name, cmd.help());
        }
        println!("");
    }
}

fn print_prompt() {
    print!("> ");
    let _ = io::stdout().flush();
}

#[derive(Default)]
struct CmdMem {
}

impl Cmd for CmdMem {
    fn help(&self) -> &'static str {
        // TODO
        "cmd mem help"
    }
    fn name(&self) -> &'static str {
        "mem"
    }
    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]) {
        // TODO
        let _ = gbc;
        let _ = cmd;
        println!("cmd mem");
    }
}

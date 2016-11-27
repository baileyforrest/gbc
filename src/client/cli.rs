extern crate libgbc;

use std::collections::BTreeMap;
use std::io::{self, Read, Write};
use std::io::BufRead;
use std::num::ParseIntError;

trait Cmd {
    fn help(&self) -> &'static str;
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
        self.cmds.insert("reg".to_string(), Box::new(CmdReg {}));
        self.cmds.insert("mem".to_string(), Box::new(CmdMem {}));
        self.cmds.insert("step".to_string(), Box::new(CmdStep {}));
        self.cmds.insert("run".to_string(), Box::new(CmdRun {}));
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
        let line_empty = line == "\n";
        let cmd_str = if line_empty {
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

        if !line_empty {
            self.last_cmd = line.to_string();
        }
    }

    fn print_help(&self) {
        println!("Commands:\n");
        for (name, cmd) in &self.cmds {
            println!("{}\n{}", name, cmd.help());
        }
        println!("");
    }
}

fn print_prompt() {
    print!("> ");
    let _ = io::stdout().flush();
}

#[derive(Default)]
struct CmdReg {
}

impl Cmd for CmdReg {
    fn help(&self) -> &'static str {
        "\
        \tget\n\
        "
    }

    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]) {
        let _ = cmd;
        println!("{}", gbc.cpu_regs());
    }
}

#[derive(Default)]
struct CmdMem {
}

impl Cmd for CmdMem {
    fn help(&self) -> &'static str {
        "\
        \tget addr [count] \n\
        \tset addr val [...val]\n\
        "
    }

    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]) {
        if cmd.len() < 2 {
            print_usage(self as &Cmd);
            return;
        }

        let addr = match parse_u16(cmd[1]) {
            Err(e) => {
                println!("Failed to parse {}: {}", cmd[0], e);
                return;
            }
            Ok(val) => val,
        };

        match cmd[0] {
            "get" => {
                println!("0x{:04x}: 0x{:02x}", addr, gbc.mem_val(addr));
                // TODO: Support read multiple
            }
            "set" => {
                // TODO: Support this
            }
            _ => {
                println!("Unknown option {}", cmd[0]);
                print_usage(self as &Cmd);
            }
        }
    }
}

#[derive(Default)]
struct CmdStep {
}

impl Cmd for CmdStep {
    fn help(&self) -> &'static str {
        "\
        \t [count] \n\
        "
    }

    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]) {
        gbc.step_instruction();
        // TODO: Remove this after debug.
        println!("{}", gbc.cpu_regs());
    }
}

#[derive(Default)]
struct CmdRun {
}

// TODO: Have this run with correct clock frequency. Implement proper breakpoints.
impl Cmd for CmdRun {
    fn help(&self) -> &'static str {
        "\
        \t pc \n\
        "
    }

    fn run(&mut self, gbc: &mut libgbc::Gbc, cmd: &[&str]) {
        if cmd.len() != 1 {
            println!("Invalid number of args");
            print_usage(self as &Cmd);
            return;
        }

        let addr = match parse_u16(cmd[0]) {
            Err(e) => {
                println!("Failed to parse {}: {}", cmd[0], e);
                return;
            }
            Ok(val) => val,
        };

        loop {
            let regs = gbc.cpu_regs();
            if regs.pc == addr {
                break;
            }

            // TODO: Remove
            // println!("{}", regs);
            gbc.step_instruction();
        }

        // TODO: Remove this after debug.
        println!("{}", gbc.cpu_regs());
    }
}


fn print_usage(cmd: &Cmd) {
    println!("Usage: \n{}", cmd.help());
}

fn parse_u8(s: &str) -> Result<u8, ParseIntError> {
    if s.len() >= 2 && &s[..2] == "0x" {
        u8::from_str_radix(&s[2..], 16)
    } else {
        u8::from_str_radix(&s, 10)
    }
}

fn parse_u16(s: &str) -> Result<u16, ParseIntError> {
    if s.len() >= 2 && &s[..2] == "0x" {
        u16::from_str_radix(&s[2..], 16)
    } else {
        u16::from_str_radix(&s, 10)
    }
}

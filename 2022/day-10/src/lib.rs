use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub struct Device {
    instructions: Vec<Instruction>,
    cycles_total: i64,
    reg_x: i64,
}

impl Device {
    pub fn initialize<P>(path: P) -> io::Result<Self>
    where
        P: AsRef<Path>,
    {
        let instructions = parse_input(path)?;
        Ok(Self {
            instructions,
            cycles_total: 0,
            reg_x: 1,
        })
    }

    pub fn for_each_cycle<F>(&mut self, mut cycle_fn: F)
    where
        F: FnMut(&mut i64, i64),
    {
        for instruction in &self.instructions {
            for _ in 0..instruction.cycles() {
                cycle_fn(&mut self.cycles_total, self.reg_x);
            }
            self.reg_x += instruction.value();
        }
    }
}

enum Instruction {
    Noop,
    Addx(i64),
}

impl Instruction {
    pub fn cycles(&self) -> i64 {
        match self {
            Self::Noop => 1,
            Self::Addx(_) => 2,
        }
    }

    pub fn value(&self) -> i64 {
        match self {
            Self::Noop => 0,
            Self::Addx(val) => *val,
        }
    }
}

fn parse_input<P>(path: P) -> io::Result<Vec<Instruction>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let mut instructions = vec![];

    for line in BufReader::new(file).lines() {
        let line = line?;
        let parts = line.split_whitespace().collect::<Vec<_>>();

        let instr = match parts[..] {
            ["addx", val] => Instruction::Addx(val.parse().unwrap()),
            ["noop"] => Instruction::Noop,
            [..] => panic!("Invalid instruction"),
        };

        instructions.push(instr);
    }

    Ok(instructions)
}

use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub enum Instruction {
    Noop,
    Addx(i64),
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Instruction>>
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

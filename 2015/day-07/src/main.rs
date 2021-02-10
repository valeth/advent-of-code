mod circuit;

use std::{
    io::{self, BufRead},
    fs::File,
};
use circuit::Circuit;

const FILE_PATHS: &[&str] = &[
    "inputs/test.txt",
    "inputs/puzzle.txt",
];

fn solve_part_1(file_path: &str) -> io::Result<u16>{
    let mut circuit = Circuit::default();

    let file = io::BufReader::new(File::open(file_path)?);
    for line in file.lines() {
        circuit.add_instruction(&line?);
    }
    circuit.execute();
    let a = circuit.variable("a").copied().unwrap_or_default();
    println!("\ta: {:?}", a);

    Ok(a)
}

fn solve_part_2(file_path: &str, a: u16) -> io::Result<()> {
    let mut circuit = Circuit::default();

    let file = io::BufReader::new(File::open(file_path)?);
    for line in file.lines() {
        circuit.add_instruction(&line?);
    }
    circuit.variable_override("b", a);
    circuit.execute();

    if let Some(a) = circuit.variable("a") {
        println!("\ta: {:?}", a);
    }

    Ok(())
}

fn main() -> io::Result<()> {
    for file_path in FILE_PATHS {
        println!("File: {}", file_path);

        let a = solve_part_1(file_path)?;
        solve_part_2(file_path, a)?;
    }

    Ok(())
}

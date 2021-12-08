mod common;

use std::env;
use std::fs::File;
use std::io::{self, BufRead};

use common::Circuit;

fn main() -> io::Result<()> {
    let path = env::args().nth(1).unwrap();
    let file = io::BufReader::new(File::open(path)?);
    let instructions = file.lines().map(|l| l.unwrap()).collect::<Vec<_>>();

    let mut circuit = Circuit::default();
    for tokens in &instructions {
        circuit.add_instruction(tokens);
    }
    circuit.execute();
    let output = circuit.variable("a").copied().unwrap();

    circuit = Circuit::default();
    for tokens in &instructions {
        circuit.add_instruction(tokens);
    }
    circuit.variable_override("b", output);
    circuit.execute();
    let output = circuit.variable("a").unwrap();

    println!("{}", output);

    Ok(())
}

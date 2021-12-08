mod common;

use std::env;
use std::fs::File;
use std::io::{self, BufRead};

use common::Circuit;

fn main() -> io::Result<()> {
    let path = env::args().nth(1).unwrap();
    let file = io::BufReader::new(File::open(path)?);

    let mut circuit = Circuit::default();
    for line in file.lines() {
        circuit.add_instruction(&line?);
    }
    circuit.execute();
    let output = circuit.variable("a").unwrap();

    println!("{}", output);

    Ok(())
}

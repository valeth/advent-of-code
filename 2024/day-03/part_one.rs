mod parser;

use std::env;

use parser::{Instruction, PuzzleInput};


fn main() {
    let input_file = env::args().nth(1).expect("file name required");

    let input = PuzzleInput::parse(input_file);

    let mut sum: u64 = 0;
    for instr in input.instructions {
        let Instruction::Mul(a, b) = instr;
        sum += a * b;
    }

    println!("{sum}");
}

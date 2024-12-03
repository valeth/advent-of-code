mod parser;

use std::env;

use parser::PuzzleInput;


fn main() {
    let input_file = env::args().nth(1).expect("file name required");

    let input = PuzzleInput::parse(input_file, true);

    let mut sum: u64 = 0;
    for (a, b) in input.instructions {
        sum += a * b;
    }

    println!("{sum}");
}

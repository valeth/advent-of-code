mod common;

use common::PuzzleInputs;


const INPUT_FILE: &str = "puzzle.txt";


fn main() {
    let mut inputs = PuzzleInputs::parse(INPUT_FILE);

    inputs.left.sort();
    inputs.right.sort();

    let mut sum = 0;
    for (l, r) in inputs.left.into_iter().zip(inputs.right) {
        let dist = l.abs_diff(r);
        sum += dist;
    }

    println!("{sum}");
}

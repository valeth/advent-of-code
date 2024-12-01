mod common;

use std::collections::HashMap;

use common::PuzzleInputs;

type Hist = HashMap<i32, i32>;


const INPUT_FILE: &str = "puzzle.txt";


fn main() {
    let inputs = PuzzleInputs::parse(INPUT_FILE);
    let mut hist = Hist::new();

    for elem in inputs.right {
        hist.entry(elem).and_modify(|e| *e += 1).or_insert(1);
    }

    let mut score = 0;

    for elem in &inputs.left {
        if let Some(x) = hist.get(elem) {
            score += elem * x;
        }
    }

    println!("{score:?}");
}

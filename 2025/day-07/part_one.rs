mod common;

use std::collections::HashSet;
use std::env;

use common::{parse, Result, Pos};


fn solve(input: Vec<Vec<Pos>>) -> u64 {
    let mut beam_positions = HashSet::<usize>::new();
    let mut split_count = 0;

    for row in input {
        for (col_idx, col) in row.into_iter().enumerate() {
            match col {
                Pos::Empty => (),
                Pos::Start => {
                    beam_positions.insert(col_idx);
                }
                Pos::Split => {
                    if let Some(beam_pos) = beam_positions.take(&col_idx) {
                        split_count += 1;
                        beam_positions.insert(beam_pos - 1);
                        beam_positions.insert(beam_pos + 1);
                    }
                }
            }
        }
    }

    split_count
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file path");
    let input = parse(infile)?;
    println!("{}", solve(input));

    Ok(())
}

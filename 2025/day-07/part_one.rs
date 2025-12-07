mod common;

use std::collections::HashSet;
use std::env;

use common::{parse, Result, Pos};


fn solve(input: Vec<Pos>) -> u64 {
    let mut beam_positions = HashSet::<usize>::new();
    let mut split_count = 0;

    for pos in input {
        match pos {
            Pos::Start(col_idx) => {
                beam_positions.insert(col_idx);
            }
            Pos::Split(col_idx) => {
                if let Some(beam_pos) = beam_positions.take(&col_idx) {
                    split_count += 1;
                    beam_positions.insert(beam_pos - 1);
                    beam_positions.insert(beam_pos + 1);
                }
            }
        }
    }

    split_count
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file path");

    let start = std::time::Instant::now();
    let input = parse(infile)?;
    let parse_time = start.elapsed();

    let start = std::time::Instant::now();
    let solution = solve(input);
    let solution_time = start.elapsed();

    let total_time = parse_time + solution_time;
    println!("Part 1:");
    println!("  Time: {parse_time:.3?} (parse) + {solution_time:.3?} (solve) = {total_time:.3?}");
    println!("  {solution}");

    Ok(())
}

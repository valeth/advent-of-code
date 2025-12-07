mod common;

use std::collections::HashMap;
use std::env;

use common::{parse, Result, Pos};


fn solve(input: Vec<Pos>) -> u64 {
    let mut outputs = HashMap::<usize, u64>::new();
    let mut start_idx = 0;

    for pos in input.into_iter().rev() {
        match pos {
            Pos::Start(col_idx) => {
                start_idx = col_idx;
            },
            Pos::Split(col_idx) => {
                let mut value = *outputs.get(&(col_idx - 1)).unwrap_or(&1);
                value += *outputs.get(&(col_idx + 1)).unwrap_or(&1);
                outputs.insert(col_idx, value);
            }
        }
    }

    outputs[&start_idx]
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
    println!("Part 2:");
    println!("  Time: {parse_time:.3?} (parse) + {solution_time:.3?} (solve) = {total_time:.3?}");
    println!("  {solution}");

    Ok(())
}

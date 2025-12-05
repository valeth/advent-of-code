mod common;

use std::env;
use common::{Result, parse, Input};


fn solve(input: Input) -> u64 {
    let mut num_fresh = 0;

    for id in input.ingredient_ids {
        if input.spoil_ranges.iter().any(|r| r.contains(&&id)) {
            num_fresh += 1;
        }
    }

    num_fresh
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let input = parse(infile)?;

    println!("{}", solve(input));

    Ok(())
}

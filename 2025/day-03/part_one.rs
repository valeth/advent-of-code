mod common;

use std::env;
use common::{Result, parse};


fn solve(banks: Vec<Vec<u32>>) -> u32 {
    let mut joltages = Vec::new();

    for bats in banks {
        let mut max = 0;

        for (idx, bat1) in bats.iter().enumerate() {
            for bat2 in &bats[idx+1..] {
                let jolt = bat1 * 10 + bat2;

                if jolt > max {
                    max = jolt;
                }
            }
        }

        joltages.push(max);
    }

    joltages.iter().sum()
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let banks = parse(infile)?;
    println!("{}", solve(banks));

    Ok(())
}

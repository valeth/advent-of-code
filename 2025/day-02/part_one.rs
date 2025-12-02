mod common;

use std::env;
use common::{parse, Result};


fn is_valid_id(input: u64) -> bool {
    let input = input.to_string();
    let (a, b) = input.split_at(input.len() / 2);
    a == b
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let ranges = parse(infile)?;

    let mut invalid_ids = Vec::new();

    for (start, end) in ranges {
        let mut test_range = 10..100;
        while test_range.start < end {
            for cur in start..=end {
                if test_range.contains(&cur) {
                    if is_valid_id(cur) {
                        invalid_ids.push(cur);
                    }
                }
            }

            test_range = (test_range.start * 100)..(test_range.end * 100);
        }
    }

    println!("{}", invalid_ids.iter().sum::<u64>());

    Ok(())
}

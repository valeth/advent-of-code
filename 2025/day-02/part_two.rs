mod common;

use std::env;
use common::{parse, Result};


fn is_valid_id(input: u64) -> bool {
    let input = input.to_string();

    let mut chunk_count = 2;
    while chunk_count < input.len() {
        if input.len() % chunk_count == 0 {
            break;
        }
        chunk_count += 1;
    }

    while chunk_count <= input.len() {
        let chunk_size = input.len() / chunk_count;

        let mut chunk_a_start = 0;
        let mut chunk_a_end = chunk_size;
        let mut chunk_b_start = chunk_a_end;
        let mut chunk_b_end = chunk_b_start + chunk_size;
        let mut repeating;

        loop {
            let chunk_a = &input[chunk_a_start..chunk_a_end];
            let chunk_b = &input[chunk_b_start..chunk_b_end];

            let chunks_equal = chunk_a == chunk_b;

            if !chunks_equal {
                repeating = false;
                break;
            }

            repeating = true;

            chunk_a_start = chunk_a_end;
            chunk_a_end = chunk_a_start + chunk_size;
            chunk_b_start = chunk_a_end;
            chunk_b_end = chunk_b_start + chunk_size;

            if chunk_b_end > input.len() {
                break;
            }
        }

        if repeating {
            return true;
        }

        chunk_count += 1;
    }

    false
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let ranges = parse(infile)?;

    let mut invalid_ids = Vec::new();

    for (start, end) in ranges {
        for cur in start..=end {
            if is_valid_id(cur) {
                invalid_ids.push(cur);
            }
        }
    }

    println!("{}", invalid_ids.iter().sum::<u64>());

    Ok(())
}

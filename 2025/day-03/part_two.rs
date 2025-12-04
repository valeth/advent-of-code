mod common;

use std::env;
use common::{Result, parse};


fn slice_to_number(slice: &[u32]) -> u64 {
    slice.iter()
        .rev()
        .enumerate()
        .map(|(i, v)| *v as u64 * 10u64.pow(i as u32))
        .sum()
}


fn joltage_for_bank(bank: &[u32]) -> u64 {
    const REQ_BATS: usize = 12;

    let mut slots = [0u32; REQ_BATS];
    let mut slot_idx = 0;
    let mut min_idx = 0;
    let mut max_idx = bank.len() - REQ_BATS;

    while slot_idx < slots.len() {
        let mut last_idx = 0;

        for (idx, bat) in bank.iter().enumerate() {
            if idx > max_idx || idx < min_idx {
                continue;
            }

            if slots[slot_idx] < *bat || slots[slot_idx] == *bat && idx <= last_idx {
                slots[slot_idx] = *bat;
                last_idx = idx;
            }
        }

        min_idx = last_idx + 1;
        slot_idx += 1;
        max_idx += 1;
    }

    slice_to_number(slots.as_ref())
}


fn solve(banks: Vec<Vec<u32>>) -> u64 {
    let mut sum = 0;

    for bats in banks {
        sum += joltage_for_bank(&bats);
    }

    sum
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let banks = parse(infile)?;
    println!("{}", solve(banks));

    Ok(())
}

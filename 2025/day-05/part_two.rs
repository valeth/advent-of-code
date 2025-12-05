mod common;

use std::collections::VecDeque;
use std::{env, cmp};
use common::{Result, parse, Range};


fn merge_ranges(ranges: Vec<Range>) -> Vec<Range> {
    let mut ranges = VecDeque::from(ranges);

    let mut merged = Vec::new();

    loop {
        match (ranges.pop_front(), ranges.pop_front()) {
            (Some(range1), Some(range2)) => {
                if range1.end() >= range2.start() {
                    let max_end = cmp::max(range1.end(), range2.end());
                    let new_range = Range::new(*range1.start(), *max_end);
                    ranges.push_front(new_range);
                } else {
                    merged.push(range1);
                    ranges.push_front(range2);
                }
            }
            (Some(range1), None) => {
                merged.push(range1);
            }
            (None, Some(_)) => unreachable!(),
            (None, None) => break
        }
    }

    merged
}


fn solve(mut ranges: Vec<Range>) -> u64 {
    ranges.sort_by(|a, b| a.start().cmp(b.start()));

    merge_ranges(ranges)
        .into_iter()
        .map(|range| range.count() as u64)
        .sum()
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();
    let input = parse(infile)?;

    println!("{}", solve(input.spoil_ranges));

    Ok(())
}

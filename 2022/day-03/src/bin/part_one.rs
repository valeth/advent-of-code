use std::{collections::HashSet, env, io};

use aoc_2022_03::{item_priority, parse_file};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let sum_of_items: u32 = parse_file(infile_path)?
        .map(|line| {
            let (left, right) = line.split_at(line.len() / 2);
            let left: HashSet<_> = left.chars().collect();
            let right: HashSet<_> = right.chars().collect();
            left.intersection(&right)
                .fold(0u32, |acc, &item| acc + item_priority(item))
        })
        .sum();

    println!("{sum_of_items}");

    Ok(())
}

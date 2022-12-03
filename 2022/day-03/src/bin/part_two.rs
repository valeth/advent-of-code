use std::{collections::HashSet, env, io};

use itertools::Itertools; // for easier chunking

use aoc_2022_03::{item_priority, parse_file};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let items = parse_file(infile_path)?.chunks(3);
    let item_sum: u32 = items
        .into_iter()
        .map(|chunk| {
            let badge_set = chunk
                .map(|line| line.chars().collect::<HashSet<_>>())
                .reduce(|acc, item| acc.intersection(&item).copied().collect())
                .unwrap();

            let badge_item = badge_set.iter().nth(0).unwrap();
            item_priority(*badge_item)
        })
        .sum();

    println!("{item_sum}");

    Ok(())
}

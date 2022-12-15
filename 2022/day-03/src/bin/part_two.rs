use std::{collections::HashSet, env, io};

use aoc_2022_03::{item_priority, parse_file};
use itertools::Itertools; // for easier chunking

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let items = parse_file(path)?.chunks(3);
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

    Ok(item_sum)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(70, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(2681, result);

        Ok(())
    }
}

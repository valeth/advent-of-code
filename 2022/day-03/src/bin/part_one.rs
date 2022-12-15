use std::{collections::HashSet, env, io};

use aoc_2022_03::{item_priority, parse_file};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let sum_of_items: u32 = parse_file(path)?
        .map(|line| {
            let (left, right) = line.split_at(line.len() / 2);
            let left: HashSet<_> = left.chars().collect();
            let right: HashSet<_> = right.chars().collect();
            left.intersection(&right)
                .fold(0u32, |acc, &item| acc + item_priority(item))
        })
        .sum();

    Ok(sum_of_items)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(157, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(8349, result);

        Ok(())
    }
}

use std::{env, io};

use aoc_2022_13::parse_input;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let pairs = parse_input(path)?;
    let result = pairs
        .iter()
        .enumerate()
        .filter_map(|(i, (a, b))| if a < b { Some(i + 1) } else { None })
        .sum::<usize>();

    Ok(result as u32)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(13, result);
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(5938, result);
    }
}

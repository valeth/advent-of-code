use std::{env, io};

use aoc_2022_07::{parse_input, path_sizes, DirMap};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<usize> {
    let paths = parse_input(path)?;

    let mut sizes = DirMap::new();
    path_sizes(&paths, &mut sizes);

    let sum = sizes.values().filter(|&&val| val <= 100_000).sum();

    Ok(sum)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(95437, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(1428881, result);

        Ok(())
    }
}

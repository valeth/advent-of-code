use std::{env, io};

use aoc_2022_07::{parse_input, path_sizes, DirMap};

const MAX_DISK_SPACE: usize = 70_000_000;
const REQUIRED_SIZE: usize = 30_000_000;

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

    let used_disk_space = *sizes.get("/").unwrap();
    let free_disk_space = MAX_DISK_SPACE - used_disk_space;
    let req_disk_space = REQUIRED_SIZE - free_disk_space;

    let values = sizes
        .values()
        .filter(|&&v| v >= req_disk_space)
        .collect::<Vec<_>>();

    let smol = values.iter().min().unwrap();

    Ok(**smol)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(24933642, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(10475598, result);

        Ok(())
    }
}

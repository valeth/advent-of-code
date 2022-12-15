use std::{env, io};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let mut sums = aoc_2022_01::input_sums(path)?.collect::<Vec<_>>();

    sums.sort_by(|a, b| b.cmp(a));

    let three_highest_sum: &u32 = &sums[..3].iter().sum();

    Ok(*three_highest_sum)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(45000, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(200158, result);

        Ok(())
    }
}

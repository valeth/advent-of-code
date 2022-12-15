use std::{env, io};

use aoc_2022_04::{parse_inputs, Section};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<usize> {
    let inputs = parse_inputs(path)?;

    let count = inputs.into_iter().filter(sections_overlap).count();

    println!("{count}");

    Ok(count)
}

fn sections_overlap((Section(l1, l2), Section(r1, r2)): &(Section, Section)) -> bool {
    (l1 >= r1 && l2 <= r2) || (r1 >= l1 && r2 <= l2)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(2, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(569, result);

        Ok(())
    }
}

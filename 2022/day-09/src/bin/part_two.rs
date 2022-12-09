use std::{env, io};

use aoc_2022_09::{parse_input, Rope, Visited};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<usize> {
    let instructions = parse_input(path)?;
    let mut rope = Rope::new(10);
    let mut tail_visited = Visited::new();

    tail_visited.insert(rope.tail().unwrap().clone());

    for instruction in instructions {
        instruction.apply_to_rope(&mut rope, &mut tail_visited);
    }

    Ok(tail_visited.len())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(1, result);

        let result = solve("inputs/test2.txt").unwrap();
        assert_eq!(36, result);
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(2273, result);
    }
}

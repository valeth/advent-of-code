use std::{env, io};

use aoc_2022_05::{parse_file, Instruction, Stack, Stacks};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<String> {
    let (mut stacks, instructions) = parse_file(path)?;

    for instruction in instructions {
        move_stack(&mut stacks, instruction);
    }

    let top_of_stacks = stacks
        .values()
        .map(|stack| stack.front().unwrap())
        .collect();

    Ok(top_of_stacks)
}

pub fn move_stack(stacks: &mut Stacks, (amount, from_idx, to_idx): Instruction) {
    let source = stacks.get_mut(&from_idx).unwrap();
    let mut swap = Stack::new();
    for _ in 0..amount {
        swap.push_front(source.pop_front().unwrap());
    }

    let target = stacks.get_mut(&to_idx).unwrap();

    for item in swap {
        target.push_front(item);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!("MCD", result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!("GGNPJBTTR", result);

        Ok(())
    }
}

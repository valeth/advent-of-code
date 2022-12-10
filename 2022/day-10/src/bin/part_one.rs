use std::{env, io};

use aoc_2022_10::{parse_input, Instruction};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<i64> {
    let instructions = parse_input(path)?;

    let mut cycles_total = 0;
    let mut reg_x = 1;
    let mut sig_strength_total = 0;

    for instruction in instructions {
        let (cycles, add_x) = match instruction {
            Instruction::Noop => (1, 0),
            Instruction::Addx(val) => (2, val),
        };

        for _ in 0..cycles {
            cycles_total += 1;

            if cycles_total == 20 || (cycles_total - 20) % 40 == 0 {
                let sig_strength = cycles_total * reg_x;
                sig_strength_total += sig_strength;
            }
        }

        reg_x += add_x;
    }

    Ok(sig_strength_total)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(13140, result)
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(13760, result);
    }
}

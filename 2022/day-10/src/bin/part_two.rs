use std::{env, io};

use aoc_2022_10::{parse_input, Instruction};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

const PIXEL_LIT: char = '#';
const PIXEL_UNLIT: char = '.';
const DRAW_CYCLE: i64 = 40;

fn solve(path: &str) -> io::Result<String> {
    let instructions = parse_input(path)?;

    let mut crt = Vec::<[char; 40]>::new();
    let mut crt_row = [PIXEL_UNLIT; 40];

    let mut cycles_total = 0;
    let mut reg_x = 1_i64;

    for instruction in instructions {
        let (cycles, add_x) = match instruction {
            Instruction::Noop => (1, 0),
            Instruction::Addx(val) => (2, val),
        };

        for _ in 0..cycles {
            let cur_row_pos = cycles_total % DRAW_CYCLE;

            if (reg_x - 1..=reg_x + 1).contains(&cur_row_pos) {
                crt_row[cur_row_pos as usize] = PIXEL_LIT;
            }

            cycles_total += 1;

            if cycles_total % DRAW_CYCLE == 0 {
                crt.push(crt_row);
                crt_row = [PIXEL_UNLIT; 40];
            }
        }

        reg_x += add_x;
    }

    let lines = crt
        .iter()
        .map(|elem| elem.iter().collect::<String>())
        .collect::<Vec<_>>()
        .join("\n");

    Ok(lines)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        const OUTPUT: &str = "\
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....";

        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(OUTPUT, &result)
    }

    #[test]
    fn puzzle() {
        const OUTPUT: &str = "\
###..####.#..#.####..##..###..####.####.
#..#.#....#.#.....#.#..#.#..#.#....#....
#..#.###..##.....#..#....#..#.###..###..
###..#....#.#...#...#....###..#....#....
#.#..#....#.#..#....#..#.#....#....#....
#..#.#....#..#.####..##..#....####.#....";

        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(OUTPUT, result);
    }
}

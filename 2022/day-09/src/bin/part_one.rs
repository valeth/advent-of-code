use std::{env, io};

use aoc_2022_09::{parse_input, Instruction, Position, VisitedMap};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<usize> {
    let instructions = parse_input(path)?;

    let mut head_pos = Position(0, 0);
    let mut tail_pos = Position(0, 0);
    let mut tail_visited = VisitedMap::new();

    tail_visited.insert(tail_pos.clone(), 1);

    for instruction in instructions {
        (head_pos, tail_pos) =
            apply_instruction(&instruction, &head_pos, &tail_pos, &mut tail_visited);
    }

    Ok(tail_visited.len())
}

fn apply_instruction(
    instruction: &Instruction,
    head_pos: &Position,
    tail_pos: &Position,
    tail_visited: &mut VisitedMap,
) -> (Position, Position) {
    let mut tmp_head_pos = head_pos.clone();
    let mut tmp_tail_pos = tail_pos.clone();

    let (change, amount) = match *instruction {
        Instruction::Up(dist) => (Position(1, 0), dist),
        Instruction::Right(dist) => (Position(0, 1), dist),
        Instruction::Left(dist) => (Position(0, -1), dist),
        Instruction::Down(dist) => (Position(-1, 0), dist),
    };

    for _ in 1..=amount {
        tmp_head_pos += change.clone();

        let dx = tmp_head_pos.0 - tmp_tail_pos.0;
        let dxa = dx.abs();
        let dy = tmp_head_pos.1 - tmp_tail_pos.1;
        let dya = dy.abs();
        let same_row = tmp_head_pos.0 == tmp_tail_pos.0;
        let same_col = tmp_head_pos.1 == tmp_tail_pos.1;
        let mut head_changed = false;

        if (!same_row && !same_col) && (dxa > 1 || dya > 1) {
            let x = if dx.is_negative() { -1 } else { 1 };
            let y = if dy.is_negative() { -1 } else { 1 };
            tmp_tail_pos += Position(x, y);
            head_changed = true;
        } else if dxa > 1 || dya > 1 {
            tmp_tail_pos += change.clone();
            head_changed = true;
        }

        if head_changed {
            tail_visited
                .entry(tmp_tail_pos.clone())
                .and_modify(|v| *v += 1)
                .or_insert(1);
        }
    }

    (tmp_head_pos, tmp_tail_pos)
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
        assert_eq!(6026, result);
    }
}

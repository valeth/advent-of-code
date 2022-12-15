use std::{env, io};

use aoc_2022_02::{calculate_round_score, parse_lines, Player, RoundResult};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u64> {
    let rounds = parse_lines(path)?.map(|(a, b)| (Player::parse(&a), RoundResult::parse(&b)));

    let mut total_score = 0u64;

    for (enemy, expected_result) in rounds {
        let myself = enemy.versus(&expected_result);
        total_score += calculate_round_score(&myself, &enemy);
    }

    Ok(total_score)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(12, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(12091, result);

        Ok(())
    }
}

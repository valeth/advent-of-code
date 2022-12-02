use std::{env, io};

use aoc_2022_02::{calculate_round_score, parse_lines, Player, RoundResult};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let rounds =
        parse_lines(infile_path)?.map(|(a, b)| (Player::parse(&a), RoundResult::parse(&b)));

    let mut total_score = 0u64;

    for (enemy, expected_result) in rounds {
        let myself = enemy.versus(&expected_result);
        total_score += calculate_round_score(&myself, &enemy);
    }

    println!("{total_score}");

    Ok(())
}

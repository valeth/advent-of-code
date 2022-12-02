use std::{env, io};

use aoc_2022_02::{calculate_round_score, parse_lines, Player};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let rounds = parse_lines(infile_path)?.map(|(a, b)| (Player::parse(&a), Player::parse(&b)));

    let mut total_score = 0u64;

    for (enemy, myself) in rounds {
        total_score += calculate_round_score(&myself, &enemy);
    }

    println!("{total_score}");

    Ok(())
}

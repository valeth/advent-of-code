use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

#[derive(Debug, Clone, Copy)]
pub enum Player {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

impl Player {
    pub fn wins_against(&self, other: &Self) -> RoundResult {
        use Player::*;
        use RoundResult::*;

        match (self, other) {
            (Rock, Paper) => Loss,
            (Rock, Scissors) => Win,
            (Paper, Rock) => Win,
            (Paper, Scissors) => Loss,
            (Scissors, Rock) => Loss,
            (Scissors, Paper) => Win,
            _ => RoundResult::Draw,
        }
    }

    pub fn versus(&self, expected_result: &RoundResult) -> Self {
        use Player::*;
        use RoundResult::*;

        match (expected_result, self) {
            (Win, Rock) => Paper,
            (Win, Paper) => Scissors,
            (Win, Scissors) => Rock,
            (Loss, Rock) => Scissors,
            (Loss, Paper) => Rock,
            (Loss, Scissors) => Paper,
            (Draw, other) => *other,
        }
    }

    pub fn parse(val: &str) -> Self {
        match val {
            "A" | "X" => Self::Rock,
            "B" | "Y" => Self::Paper,
            "C" | "Z" => Self::Scissors,
            _ => panic!("Invalid input"),
        }
    }
}

#[derive(Debug)]
pub enum RoundResult {
    Loss = 0,
    Draw = 3,
    Win = 6,
}

impl RoundResult {
    pub fn parse(val: &str) -> Self {
        match val {
            "X" => Self::Loss,
            "Y" => Self::Draw,
            "Z" => Self::Win,
            _ => panic!("Invalid input"),
        }
    }
}

pub fn calculate_round_score(player1: &Player, player2: &Player) -> u64 {
    (*player1 as u64) + (player1.wins_against(&player2) as u64)
}

pub fn parse_lines<P>(path: P) -> io::Result<impl Iterator<Item = (String, String)>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let rounds = reader.lines().map(|line| {
        let line = line.unwrap();
        let pair: Vec<_> = line.split_whitespace().collect();
        (pair[0].into(), pair[1].into())
    });

    Ok(rounds)
}

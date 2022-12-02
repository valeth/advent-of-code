use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

#[derive(Debug, Clone, Copy)]
pub enum Player {
    Rock,
    Paper,
    Scissors,
}

impl Player {
    pub fn shape_score(&self) -> u64 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }

    pub fn wins_against(&self, other: &Self) -> RoundResult {
        match (self, other) {
            (Self::Rock, Self::Paper) => RoundResult::Loss,
            (Self::Rock, Self::Scissors) => RoundResult::Win,
            (Self::Paper, Self::Rock) => RoundResult::Win,
            (Self::Paper, Self::Scissors) => RoundResult::Loss,
            (Self::Scissors, Self::Rock) => RoundResult::Loss,
            (Self::Scissors, Self::Paper) => RoundResult::Win,
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
    Win,
    Draw,
    Loss,
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
    player1.shape_score()
        + match player1.wins_against(&player2) {
            RoundResult::Win => 6,
            RoundResult::Draw => 3,
            RoundResult::Loss => 0,
        }
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

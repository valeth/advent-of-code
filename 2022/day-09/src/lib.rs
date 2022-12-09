use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead, BufReader},
    ops::{Add, AddAssign},
    path::Path,
    str::FromStr,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position(pub isize, pub isize);

impl Add for Position {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0, self.1 + rhs.0)
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

pub type VisitedMap = HashMap<Position, u32>;

#[derive(Debug)]
pub enum Instruction {
    Up(isize),
    Right(isize),
    Left(isize),
    Down(isize),
}

impl FromStr for Instruction {
    type Err = String;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        let parts = val.split_whitespace().collect::<Vec<&str>>();
        let [direction, distance, ..] = parts[..] else {
            return Err("Invalid instruction".to_string());
        };
        let distance = distance
            .parse()
            .map_err(|_| "Invalid distance value".to_string())?;

        let result = match direction {
            "U" => Self::Up(distance),
            "R" => Self::Right(distance),
            "L" => Self::Left(distance),
            "D" => Self::Down(distance),
            _ => return Err("Invalid direction value".to_string()),
        };

        Ok(result)
    }
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Instruction>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let instructions = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();

    Ok(instructions)
}

use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

pub type WorryLevel = u64;
pub type MonkeyId = usize;

#[derive(Debug)]
pub enum Param {
    Old,
    Val(WorryLevel),
}

#[derive(Debug)]
pub enum Operation {
    Mul(Param, Param),
    Add(Param, Param),
    Noop,
}

impl Operation {
    pub fn apply(&self, old: WorryLevel) -> WorryLevel {
        match self {
            Self::Mul(Param::Old, Param::Val(val)) => old * val,
            Self::Mul(Param::Old, Param::Old) => old * old,
            Self::Add(Param::Old, Param::Val(val)) => old + val,
            Self::Add(Param::Old, Param::Old) => old + old,
            _ => panic!("unsupported operation"),
        }
    }
}

#[derive(Debug)]
pub struct Note {
    pub items: Vec<WorryLevel>,
    pub operation: Operation,
    pub test_mod: WorryLevel,
    pub next_monkey_when_true: MonkeyId,
    pub next_monkey_when_false: MonkeyId,
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Note>>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let notes = input.split("\n\n").map(|part| parse_note(part)).collect();

    Ok(notes)
}

fn parse_note(input: &str) -> Note {
    let mut items = vec![];
    let mut operation = Operation::Noop;
    let mut test_mod = 0;
    let mut next_monkey_when_true = 0;
    let mut next_monkey_when_false = 0;

    for line in input.lines() {
        let line = line.trim_start();
        let (val, rem) = line.split_once(':').unwrap();

        match val {
            "Starting items" => {
                items = parse_starting_items(rem);
            }
            "Operation" => {
                operation = parse_operation(rem);
            }
            "Test" => {
                test_mod = parse_test(rem);
            }
            "If true" => {
                next_monkey_when_true = parse_next_monkey(rem);
            }
            "If false" => {
                next_monkey_when_false = parse_next_monkey(rem);
            }
            _ => (),
        }
    }

    Note {
        items,
        operation,
        test_mod,
        next_monkey_when_true,
        next_monkey_when_false,
    }
}

fn parse_starting_items(input: &str) -> Vec<WorryLevel> {
    input
        .split(',')
        .map(|val| val.trim_start().parse().unwrap())
        .collect()
}

fn parse_operation(input: &str) -> Operation {
    let input = input.strip_prefix(" new = old ").unwrap();
    let op_and_val = input.split_whitespace().collect::<Vec<&str>>();
    match *op_and_val {
        ["*", "old"] => Operation::Mul(Param::Old, Param::Old),
        ["*", val] => {
            let val = val.parse().unwrap();
            Operation::Mul(Param::Old, Param::Val(val))
        }
        ["+", "old"] => Operation::Add(Param::Old, Param::Old),
        ["+", val] => {
            let val = val.parse().unwrap();
            Operation::Add(Param::Old, Param::Val(val))
        }
        _ => panic!("Unsupported operation"),
    }
}

fn parse_test(input: &str) -> WorryLevel {
    input
        .strip_prefix(" divisible by ")
        .unwrap()
        .parse()
        .unwrap()
}

fn parse_next_monkey(input: &str) -> MonkeyId {
    input
        .strip_prefix(" throw to monkey ")
        .unwrap()
        .parse()
        .unwrap()
}

use std::fs;
use std::path::Path;


#[derive(Debug)]
pub struct PuzzleInputs {
    pub left: Vec<i32>,
    pub right: Vec<i32>,
}

impl PuzzleInputs {
    pub fn parse<P>(path: P) -> Self
        where P: AsRef<Path>
    {
        let mut left = Vec::new();
        let mut right = Vec::new();

        let content = fs::read_to_string(path).unwrap();

        for line in content.lines() {
            let mut parts = line.split(' ');

            // unwrap is very festive
            let first = parts.next().unwrap();
            let first = first.parse().unwrap();
            let last = parts.last().unwrap();
            let last = last.parse().unwrap();

            left.push(first);
            right.push(last);
        }

        Self { left, right }
    }
}

use std::fs;
use std::path::Path;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pos {
    Start,
    Split,
    Empty,
}


pub fn parse<P>(path: P) -> Result<Vec<Vec<Pos>>>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;

    let parsed = content.lines().map(|line| {
        line.chars().map(|ch| match ch {
            'S' => Pos::Start,
            '.' => Pos::Empty,
            '^' => Pos::Split,
            _ => panic!("invalid char")
        }).collect()
    }).collect();

    Ok(parsed)
}

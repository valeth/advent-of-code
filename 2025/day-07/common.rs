use std::fs;
use std::path::Path;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


#[derive(Debug, Clone, Copy)]
pub enum Pos {
    Start(usize),
    Split(usize),
}


pub fn parse<P>(path: P) -> Result<Vec<Pos>>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;

    let parsed = content.lines().flat_map(|line| {
        line.chars().enumerate().filter_map(move |(col_idx, ch)| match ch {
            'S' => Some(Pos::Start(col_idx)),
            '^' => Some(Pos::Split(col_idx)),
            '.' => None,
            _ => panic!("invalid char")
        })
    }).collect();

    Ok(parsed)
}

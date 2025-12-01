use std::io::BufRead;
use std::path::Path;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


pub const MAX_NUMBER: i32 = 99;
pub const MIN_NUMBER: i32 = 0;


pub fn parse_file<A>(path: A) -> Result<Vec<i32>>
where
    A: AsRef<Path>,
{
    let mut instructions = Vec::new();

    let content = std::fs::read(path)?;

    for line in content.lines() {
        let line = line?;
        let (direction, amount) = line.split_at(1);
        let amount = amount.parse::<i32>()?;
        let instruction = match direction {
            "R" => amount,
            "L" => -amount,
            _ => panic!("invalid direction"),
        };
        instructions.push(instruction);
    }

    Ok(instructions)
}


pub fn apply_instruction<const COUNT_VISITED: bool>(cur_pos: i32, amount: i32) -> (i32, u64) {
    let mut zeros = 0;

    if COUNT_VISITED {
        zeros += amount.abs() / (MAX_NUMBER + 1);
    }

    let remaining = amount % (MAX_NUMBER + 1);
    let next_pos = cur_pos + remaining;

    let pos = if next_pos < MIN_NUMBER {
        if COUNT_VISITED && cur_pos != 0 {
            zeros += 1;
        }
        MAX_NUMBER + next_pos + 1
    } else if next_pos > MAX_NUMBER {
        if COUNT_VISITED && cur_pos != 0 {
            zeros += 1;
        }
        next_pos - MAX_NUMBER - 1
    } else if COUNT_VISITED && next_pos == 0 {
        zeros += 1;
        next_pos
    } else {
        next_pos
    };

    (pos, zeros as u64)
}

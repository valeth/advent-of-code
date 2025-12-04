use std::fs;
use std::path::Path;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


fn parse_char(ch: char) -> u32 {
    ch.to_digit(10).unwrap() as u32
}


pub fn parse<P>(path: P) -> Result<Vec<Vec<u32>>>
    where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;
    let mut battery_banks = Vec::new();

    for line in content.lines() {
        let batteries = line.chars().map(parse_char).collect();
        battery_banks.push(batteries);
    }

    Ok(battery_banks)
}

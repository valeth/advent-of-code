use std::path::Path;
use std::fs;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;
pub type Range = std::ops::RangeInclusive<u64>;


#[derive(Debug)]
#[allow(unused)]
pub struct Input {
    pub spoil_ranges: Vec<Range>,
    pub ingredient_ids: Vec<u64>,
}


pub fn parse<P>(path: P) -> Result<Input>
where P: AsRef<Path>
{
    let mut parse_ranges = true;

    let mut spoil_ranges = Vec::new();
    let mut ingredient_ids = Vec::new();

    let content = fs::read_to_string(path)?;

    let lines = content.lines();

    for line in lines {
        if parse_ranges && line.is_empty() {
            parse_ranges = false;
            continue;
        }

        if parse_ranges {
            let (start, end) = line.split_once('-').unwrap();
            let start = start.parse::<u64>().unwrap();
            let end = end.parse::<u64>().unwrap();
            spoil_ranges.push(start..=end);
        } else {
            let id = line.parse().unwrap();
            ingredient_ids.push(id);
        }
    }

    Ok(Input { spoil_ranges, ingredient_ids })
}

use std::path::Path;
use std::fs;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


pub fn parse<P>(path: P) -> Result<Vec<(u64, u64)>>
where P: AsRef<Path>
{
    let mut ranges = Vec::new();

    let content = fs::read_to_string(path)?;

    for range in content.split(',') {
        let Some((start, end)) = range.split_once('-') else {
            return Err(format!("invalid range: {range:?}",).into());
        };
        let range = (start.parse()?, end.parse()?);
        ranges.push(range);
    }

    Ok(ranges)
}


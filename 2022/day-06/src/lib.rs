use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

use itertools::Itertools;

pub fn find_marker(inputs: &[char], length: usize) -> usize {
    for (idx, chunk) in inputs.windows(length).enumerate() {
        let dups = chunk.iter().duplicates().collect::<Vec<_>>();
        if dups.is_empty() {
            return idx + length;
        }
    }

    panic!("how did we get here, whoops");
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<char>>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let chars = input.chars().collect();
    Ok(chars)
}

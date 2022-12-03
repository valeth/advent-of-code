use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub fn item_priority(item_id: char) -> u32 {
    if item_id.is_ascii_lowercase() {
        (item_id as u32) - 96
    } else if item_id.is_ascii_uppercase() {
        (item_id as u32) - 38
    } else {
        panic!("unsupported ascii");
    }
}

pub fn parse_file<P>(path: P) -> io::Result<impl Iterator<Item = String>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let items = BufReader::new(file).lines().map(|line| {
        let line = line.expect("got no line");
        line
    });

    Ok(items)
}

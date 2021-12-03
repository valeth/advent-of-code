use std::env;
use std::fs::File;
use std::io::Read;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn read_file() -> Result<Vec<u32>> {
    let path = env::args().skip(1).next().unwrap();

    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    Ok(content.lines().map(|l| l.parse().unwrap()).collect())
}

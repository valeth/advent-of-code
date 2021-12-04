use std::env;
use std::fs::File;
use std::io::Read;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn read_file() -> Result<Vec<(usize, usize, char, String)>> {
    let path = env::args().skip(1).next().unwrap();
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let result = content
        .lines()
        .map(|line| {
            let tokens = line
                .split(&[' ', ':', '-'][..])
                .filter(|&x| x != "")
                .collect::<Vec<_>>();
            (
                tokens[0].parse().unwrap(),
                tokens[1].parse().unwrap(),
                tokens[2].chars().next().unwrap(),
                tokens[3].into(),
            )
        })
        .collect();

    Ok(result)
}

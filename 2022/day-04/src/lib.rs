use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

// just impl Copy here, it'll be fine TM
#[derive(Debug, Clone, Copy)]
pub struct Section(pub u32, pub u32);

pub fn parse_inputs<P>(path: P) -> io::Result<Vec<(Section, Section)>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let inputs = reader
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let parts = line
                .split(",")
                .map(|sections| {
                    let section = sections
                        .split("-")
                        .map(|num| num.parse::<u32>().unwrap())
                        .collect::<Vec<_>>();
                    Section(section[0], section[1])
                })
                .collect::<Vec<_>>();
            (parts[0], parts[1])
        })
        .collect::<Vec<_>>();

    Ok(inputs)
}

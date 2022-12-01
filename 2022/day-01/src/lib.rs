use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub fn input_sums<P>(path: P) -> io::Result<impl Iterator<Item = u32>>
where
    P: AsRef<Path>,
{
    let inputs = parse_input_file(path)?;

    let sums = inputs.into_iter().map(|val| val.into_iter().sum());

    Ok(sums)
}

fn parse_input_file<P>(path: P) -> io::Result<Vec<Vec<u32>>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut inputs = Vec::new();
    let mut current = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if line.is_empty() {
            inputs.push(current);
            current = Vec::new();
        } else {
            let value = line.parse().expect("invalid number");
            current.push(value);
        }
    }

    if !current.is_empty() {
        inputs.push(current);
    }

    Ok(inputs)
}

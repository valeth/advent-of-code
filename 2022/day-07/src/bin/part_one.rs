use std::{env, io};

use aoc_2022_07::{parse_input, path_sizes, DirMap};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let paths = parse_input(infile_path)?;

    let mut sizes = DirMap::new();
    path_sizes(&paths, &mut sizes);

    let sum: usize = sizes.values().filter(|&&val| val <= 100_000).sum();

    println!("{sum:#?}");

    Ok(())
}

use std::{env, io};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let sums = aoc_2022_01::input_sums(infile_path)?;

    let max = sums.max().unwrap();
    println!("{max}");

    Ok(())
}

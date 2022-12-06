use std::{env, io};

use aoc_2022_06::{find_marker, parse_input};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let inputs = parse_input(infile_path)?;

    println!("{}", find_marker(&inputs, 4));

    Ok(())
}

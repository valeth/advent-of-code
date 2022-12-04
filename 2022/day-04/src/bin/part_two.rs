use std::{env, io};

use aoc_2022_04::{parse_inputs, Section};

fn sections_overlap((Section(l1, l2), Section(r1, r2)): &(Section, Section)) -> bool {
    (r1 >= l1 && r1 <= l2)
        || (r2 <= l2 && r2 >= l1)
        || (l1 >= r1 && l1 <= r2)
        || (l2 <= r2 && l2 >= r1)
}

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).unwrap();
    let inputs = parse_inputs(infile_path)?;

    let count = inputs.into_iter().filter(sections_overlap).count();

    println!("{count}");

    Ok(())
}

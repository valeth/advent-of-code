use std::{env, io};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");
    let mut sums = aoc_2022_01::input_sums(infile_path)?.collect::<Vec<_>>();

    sums.sort_by(|a, b| b.cmp(a));

    let three_highest_sum: &u32 = &sums[..3].iter().sum();
    println!("{three_highest_sum}");

    Ok(())
}

mod common;

use std::env;
use std::fs::read_to_string;

use common::{find_suffix_with_zeroes, Result};

fn main() -> Result<()> {
    let path = env::args().skip(1).next().unwrap();
    let input = read_to_string(path)?;
    let input = input.lines().next().unwrap().to_owned();

    let result = find_suffix_with_zeroes(&input, 5);

    println!("{}", result);

    Ok(())
}

mod common;

use std::env;

use common::{read_file, Result};

fn main() -> Result<()> {
    let path = env::args().skip(1).next().unwrap();
    let map_of_trees = read_file(path)?;

    let result: u64 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|(x, y)| map_of_trees.find_in_slope(*x, *y))
        .product();

    println!("{}", result);

    Ok(())
}

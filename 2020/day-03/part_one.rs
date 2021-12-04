mod common;

use std::env;

use common::{read_file, Result};

fn main() -> Result<()> {
    let path = env::args().skip(1).next().unwrap();
    let map_of_trees = read_file(path)?;

    let tree_count = map_of_trees.find_in_slope(3, 1);

    println!("{}", tree_count);

    Ok(())
}

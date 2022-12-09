use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub type Tree = u8;
pub type Trees = Vec<Vec<Tree>>;

pub fn parse_file<P>(path: P) -> io::Result<Trees>
where
    P: AsRef<Path>,
{
    let mut trees = Trees::new();

    let file = File::open(path)?;
    for line in BufReader::new(file).lines() {
        let line = line?;
        let tree_row = line
            .chars()
            .map(|tree| tree.to_digit(10).unwrap() as u8)
            .collect();
        trees.push(tree_row);
    }

    Ok(trees)
}

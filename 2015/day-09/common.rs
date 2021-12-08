use std::collections::HashSet;
use std::error::Error as StdError;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use std::result::Result as StdResult;

use crate::graph;

pub type Location = String;
pub type Distance = u64;
pub type Graph = graph::Graph<Location, Distance>;
pub type Node = graph::Node<Location, Distance>;
pub type NodeSet = HashSet<Node>;

pub type Error = Box<dyn StdError>;
pub type Result<T, E = Error> = StdResult<T, E>;

pub fn parse_file<P>(path: P) -> io::Result<Graph>
where
    P: AsRef<Path>,
{
    let file = BufReader::new(File::open(path)?);
    let mut directions = Graph::default();

    for line in file.lines() {
        let line = line?;
        let words = line.split(' ').collect::<Vec<_>>();
        let location = (
            words[0].to_string(),
            words[2].to_string(),
            words[4].parse::<Distance>().unwrap(),
        );

        directions.add_edge(&location);
    }

    Ok(directions)
}

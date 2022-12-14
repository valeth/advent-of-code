use std::{env, io};

use aoc_2022_12::{parse_input, Graph};
use petgraph::algo::dijkstra;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<i32> {
    let (start, end, points) = parse_input(path)?;

    let graph = Graph::from_edges(points);

    let result = dijkstra(&graph, start, Some(end), |_| 1);

    Ok(result[&end])
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(31, result)
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(420, result);
    }
}

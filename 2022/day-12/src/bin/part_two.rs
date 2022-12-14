use std::{env, io};

use aoc_2022_12::{parse_input, Graph};
use itertools::Itertools;
use petgraph::algo::dijkstra;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<i32> {
    let (_, end, points) = parse_input(path)?;

    let potential_starting_points = points
        .iter()
        .filter_map(|(a, _)| if a.2 == 1 { Some(*a) } else { None })
        .unique()
        .collect::<Vec<_>>();

    let graph = Graph::from_edges(points);
    let result = potential_starting_points
        .into_iter()
        .filter_map(|start| {
            let result = dijkstra(&graph, start, Some(end), |_| 1);
            result.get(&end).copied()
        })
        .min()
        .unwrap();

    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(29, result)
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(414, result);
    }
}

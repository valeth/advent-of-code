mod common;

use std::env;
use std::collections::{BTreeMap as Map, HashSet as Set};

use common::{parse, all_distances, Result, JunctionBoxes};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file");
    let is_puzzle = infile.contains("puzzle");

    let parse_time = std::time::Instant::now();
    let input = parse(infile)?;
    let parse_time = parse_time.elapsed();

    let solve_time = std::time::Instant::now();
    let solution = solve(input);
    let solve_time = solve_time.elapsed();

    if is_puzzle {
        assert_eq!(solution, 3926518899);
    }

    println!("Part 2:");
    println!("  Time: {parse_time:.3?} (parse) + {solve_time:.3?} (solve) = {:.3?}", parse_time + solve_time);
    println!("  {solution}");

    Ok(())
}


fn solve(boxes: JunctionBoxes) -> u64 {
    let mut circuits = Map::from_iter(
        boxes.iter().enumerate().map(|(i, _)| (i, Set::from([i])))
    );

    let distances = all_distances(&boxes);

    for (idx1, idx2, _) in distances.iter() {
        if circuits[&idx1].contains(&idx2) {
            continue;
        }

        let set1 = &circuits[&idx1];
        let set2 = &circuits[&idx2];
        let merged = set1.union(set2).copied().collect::<Set<_>>();

        if merged.len() == boxes.len() {
            return boxes[*idx1].0 * boxes[*idx2].0;
        }

        for idx in &merged {
            circuits.insert(*idx, merged.clone());
        }
    }

    panic!("not found");
}

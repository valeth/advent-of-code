mod common;

use std::env;
use std::collections::{BTreeMap as Map, HashSet as Set};

use common::{parse, all_distances, Result, JunctionBoxes};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file");
    let input = parse(infile)?;
    let solution = solve(input);
    println!("{solution}");

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

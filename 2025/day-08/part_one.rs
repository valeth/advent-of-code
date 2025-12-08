mod common;

use std::env;
use std::collections::{BTreeMap as Map, HashSet as Set};

use common::{parse, all_distances, Result, ArrayList, JunctionBoxes};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file");

    let num_pairs = if infile.contains("puzzle") {
        1000
    } else {
        10
    };

    let input = parse(infile)?;
    let solution = solve(input, num_pairs);
    println!("{solution}");

    Ok(())
}


fn solve(boxes: JunctionBoxes, num_pairs: usize) -> u32 {
    let mut circuits = Map::from_iter(
        boxes.iter().enumerate().map(|(i, _)| (i, Set::from([i])))
    );

    let distances = all_distances(&boxes);

    for (idx1, idx2, _) in distances.iter().take(num_pairs) {
        if circuits[&idx1].contains(&idx2) {
            continue;
        }

        let set1 = &circuits[&idx1];
        let set2 = &circuits[&idx2];
        let merged = set1.union(set2).copied().collect::<Set<_>>();

        for idx in &merged {
            circuits.insert(*idx, merged.clone());
        }
    }

    let mut distinct = circuits.into_iter().fold(ArrayList::new(), |mut acc, (_, circuit)| {
        if !acc.contains(&circuit) {
            acc.push(circuit);
        }
        acc
    });

    distinct.sort_by_key(|c| c.len());
    distinct.into_iter().rev().take(3).map(|c| c.len() as u32).product()
}

mod common;

use std::env;
use common::{parse, Result, Tiles};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file");
    let is_puzzle = infile.contains("puzzle");

    let start = std::time::Instant::now();
    let input = parse(infile)?;
    let parse_time = start.elapsed();

    let start = std::time::Instant::now();
    let solution = solve(input);
    let solve_time = start.elapsed();

    if is_puzzle {
        assert_eq!(solution, 4748826374);
    }

    println!("Part 1:");
    println!("  Time: {parse_time:.3?} (parse) + {solve_time:.3?} (solve) = {:.3?}", parse_time + solve_time);
    println!("  {solution}");

    Ok(())
}


fn solve(tiles: Tiles) -> u64 {
    let mut largest_area = 0;

    for (idx1, tile1) in tiles.iter().enumerate() {
        for tile2 in tiles.iter().skip(idx1 + 1) {
            let dx = tile1.x.abs_diff(tile2.x) + 1;
            let dy = tile1.y.abs_diff(tile2.y) + 1;
            let area = dx * dy;

            if largest_area < area {
                largest_area = area;
            }
        }
    }

    largest_area
}

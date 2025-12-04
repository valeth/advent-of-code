mod common;

use std::env;
use common::{Result, Grid, parse, count_adjacent};


fn solve(grid: Grid) -> u64 {
    let mut can_access = 0;

    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, cell) in row.iter().enumerate() {
            if *cell && count_adjacent(&grid, col_idx, row_idx) < 4 {
                can_access += 1;
            }
        }
    }

    can_access
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();

    let grid = parse(infile)?;

    println!("{}", solve(grid));

    Ok(())
}

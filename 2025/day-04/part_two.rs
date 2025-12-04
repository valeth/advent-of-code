mod common;

use std::env;
use common::{Result, Grid, parse, count_adjacent};


fn solve(mut grid: Grid) -> u64 {
    let mut removed_total = 0;

    loop {
        let mut new_grid = grid.clone();
        let mut removed_count = 0;

        for (row_idx, row) in grid.iter().enumerate() {
            for (col_idx, cell) in row.iter().enumerate() {
                if *cell && count_adjacent(&grid, col_idx, row_idx) < 4 {
                    new_grid[row_idx][col_idx] = false;
                    removed_count += 1;
                }
            }
        }

        if removed_count == 0 {
            break;
        }

        grid = new_grid;
        removed_total += removed_count;
    }

    removed_total
}


fn main() -> Result<()> {
    let infile = env::args().nth(1).unwrap();

    let grid = parse(infile)?;

    println!("{}", solve(grid));

    Ok(())
}

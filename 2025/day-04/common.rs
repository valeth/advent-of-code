use std::path::Path;
use std::fs;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

pub type Grid = Vec<Vec<bool>>;


pub fn parse<P>(path: P) -> Result<Grid>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;

    let grid = content.lines().map(|line| {
        line.chars().map(|ch| ch == '@').collect()
    }).collect();

    Ok(grid)
}


pub fn count_adjacent(grid: &Grid, x: usize, y: usize) -> u8 {
    const DIRECTIONS: [(isize, isize); 8] = [
        (0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (1, 1), (-1, 1), (1, -1)
    ];

    let mut adjacents = 0;
    let grid_height = grid.len() as isize;
    let grid_width = grid[0].len() as isize;
    let x = x as isize;
    let y = y as isize;

    for (off_x, off_y) in DIRECTIONS {
        if x == 0 && off_x < 0
        || y == 0 && off_y < 0
        || x + off_x >= grid_width
        || y + off_y >= grid_height
        {
            continue;
        }

        let y = (y + off_y) as usize;
        let x = (x + off_x) as usize;

        if grid[y][x] {
            adjacents += 1;
        }
    }

    adjacents
}


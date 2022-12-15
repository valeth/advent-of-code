use std::{env, io};

use aoc_2022_14::{parse_input, CaveMap, Point, SAND_ORIGIN};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<usize> {
    let mut cave_map = parse_input(path)?;

    loop {
        let pos = simulate(&cave_map);

        cave_map.put_sand(pos);

        if pos == SAND_ORIGIN {
            break;
        }
    }

    Ok(cave_map.count_sand())
}

fn simulate(cave_map: &CaveMap) -> Point {
    let mut sand_pos = SAND_ORIGIN;
    let checks = [(1, 0), (1, -1), (1, 1)];

    loop {
        let next_checks = checks.map(|(x, y)| (sand_pos.0 + x, sand_pos.1 + y));
        let mut next_pos = sand_pos;

        for pos in next_checks {
            if !cave_map.is_obstructed(&pos) {
                next_pos = pos;
                break;
            }
        }

        if next_pos == sand_pos {
            break;
        }

        sand_pos = next_pos;

        if cave_map.reached_bottom(&sand_pos) {
            break;
        }
    }

    sand_pos
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(93, result);
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(27936, result);
    }
}

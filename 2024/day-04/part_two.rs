use std::path::Path;
use std::{env, fs};


fn main() {
    let input_file = env::args().nth(1).expect("requires input file");

    let input = PuzzleInput::parse(input_file);

    let num_xmas = count_xmas(&input);

    println!("{num_xmas}");
}


fn count_xmas(input: &PuzzleInput) -> usize {
    let mut count = 0;

    for (row_off, row) in input.map.iter().enumerate() {
        for (cell_off, cell) in row.iter().enumerate() {
            if *cell == 'A' {
                if find_x(input, row_off, cell_off) {
                    count += 1;
                }
            }
        }
    }

    count
}


fn find_x(input: &PuzzleInput, row_off: usize, cell_off: usize) -> bool {
    let map = &input.map;
    let (right, bottom) = (map[0].len() as isize, map.len() as isize);

    for offset in [0, 2] {
        let x1 = (cell_off as isize + 1) - offset;
        let x2 = (cell_off as isize - 1) + offset;

        let y1 = (row_off as isize) + 1;
        let y2 = (row_off as isize) + -1;

        if x1 < 0 || x1 >= right || x2 < 0 || x2 >= right {
            return false;
        }

        if y1 < 0 || y1 >= bottom || y2 < 0 || y2 >= bottom {
            return false;
        }

        let side_a = map[y1 as usize][x1 as usize];
        let side_b = map[y2 as usize][x2 as usize];

        if !(side_a == 'M' && side_b == 'S' || side_a == 'S' && side_b == 'M') {
            return false;
        }
    }

    true
}


#[derive(Debug)]
pub struct PuzzleInput {
    map: Vec<Vec<char>>,
}

impl PuzzleInput {
    pub fn parse<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let content = fs::read_to_string(path).unwrap();

        let map = content.lines().map(|line| line.chars().collect()).collect();

        Self { map }
    }
}

use std::path::Path;
use std::{env, fs};


fn main() {
    let input_file = env::args().nth(1).expect("requires input file");

    let input = PuzzleInput::parse(input_file);

    let num_xmas = count_xmas(&input);

    println!("{num_xmas}");
}


fn count_xmas(input: &PuzzleInput) -> usize {
    let mut total_count = 0;

    for (row_off, row) in input.map.iter().enumerate() {
        for (cell_off, cell) in row.iter().enumerate() {
            if *cell == 'X' {
                if let Some(count) = trace_xmas(input, row_off, cell_off) {
                    total_count += count;
                };
            }
        }
    }

    total_count
}


fn trace_xmas(input: &PuzzleInput, row_off: usize, cell_off: usize) -> Option<usize> {
    const STEPS: usize = 3;
    const STEP_EXPECT: [char; STEPS] = ['M', 'A', 'S'];
    const DIRECTIONS: [(isize, isize); 8] = [
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
    ];

    let (right, bottom) = (input.map[0].len() as isize, input.map.len() as isize);
    let mut count = 0;

    for (x_mod, y_mod) in DIRECTIONS {
        let x_start = (cell_off as isize) + x_mod;
        let y_start = (row_off as isize) + y_mod;

        let mut found = 0;

        for step in 0..STEPS {
            let x = x_start + ((step as isize) * x_mod);
            let y = y_start + ((step as isize) * y_mod);

            if x < 0 || x >= right || y < 0 || y >= bottom {
                break;
            }

            if input.map[y as usize][x as usize] != STEP_EXPECT[step] {
                break;
            }

            found += 1;
        }

        if found == STEP_EXPECT.len() {
            count += 1;
        }
    }

    if count > 0 {
        Some(count)
    } else {
        None
    }
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

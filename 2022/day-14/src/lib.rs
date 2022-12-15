use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    path::Path,
};

use itertools::Itertools;

pub const SAND_ORIGIN: Point = (0, 500);

pub type Point = (i32, i32);
pub type Lines = Vec<Point>;

pub struct CaveMap {
    positions: HashMap<Point, char>,
    height: i32,
    width: i32,
}

impl CaveMap {
    fn new(lines: &[Lines]) -> Self {
        let mut width = 0;
        let mut height = 0;
        let mut positions = HashMap::new();

        for line in lines {
            let rocks = line.windows(2).flat_map(|w| {
                let (x1, y1) = w[0];
                let (x2, y2) = w[1];
                let xs = if x1 < x2 { x1..=x2 } else { x2..=x1 };
                let ys = if y1 < y2 { y1..=y2 } else { y2..=y1 };
                ys.cartesian_product(xs)
            });

            for (x, y) in rocks {
                if height < x {
                    height = x;
                }

                if width < y {
                    width = y;
                }

                positions.insert((x, y), '#');
            }
        }

        CaveMap {
            positions,
            height,
            width,
        }
    }

    pub fn is_in_bounds(&self, &(x, y): &Point) -> bool {
        (0..self.height).contains(&x) && (0..self.width).contains(&y)
    }

    pub fn reached_bottom(&self, &(x, _): &Point) -> bool {
        let floor_idx = self.height + 1;
        floor_idx == x
    }

    pub fn is_obstructed(&self, pos: &Point) -> bool {
        self.positions.contains_key(pos)
    }

    pub fn put_sand(&mut self, pos: Point) {
        self.positions.insert(pos, 'o');
    }

    pub fn count_sand(&self) -> usize {
        self.positions.values().filter(|&&v| v == 'o').count()
    }

    pub fn print(&self) {
        for x in 0..=self.height {
            for y in 450..=self.width {
                if (x, y) == SAND_ORIGIN {
                    print!("S");
                    continue;
                }
                let val = self.positions.get(&(x, y)).unwrap_or(&'.');
                print!("{val}");
            }
            println!();
        }
    }
}

pub fn parse_input<P>(path: P) -> io::Result<CaveMap>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let (_, lines) = parse::lines(&buf).expect("failed to parse lines");

    Ok(CaveMap::new(&lines))
}

mod parse {
    use nom::{
        bytes::complete::tag,
        character::{self, complete::newline},
        multi::separated_list1,
        sequence::separated_pair,
        IResult,
    };

    use super::Lines;

    pub(super) fn lines(input: &str) -> IResult<&str, Vec<Lines>> {
        let pairs = separated_pair(character::complete::i32, tag(","), character::complete::i32);
        let line = separated_list1(tag(" -> "), pairs);
        separated_list1(newline, line)(input)
    }
}

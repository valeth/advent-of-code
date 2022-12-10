use std::{
    collections::HashSet,
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
    str::FromStr,
};

pub type Visited = HashSet<Knot>;

#[derive(Debug)]
pub struct Rope(Vec<Knot>);

impl Rope {
    pub fn new(len: usize) -> Self {
        Self(vec![Knot::default(); len])
    }

    pub fn move_head(&mut self, change: (i32, i32)) {
        let mut knots = self.0.iter_mut();

        let mut next_a = knots.next();
        let head_knot = next_a.as_mut().expect("no head knot");
        head_knot.0 += change.0;
        head_knot.1 += change.1;

        loop {
            let mut next_b = knots.next();

            let (Some(knot_a), Some(knot_b)) = (next_a.as_mut(), next_b.as_mut()) else {
                break;
            };

            knot_b.follow(&knot_a);

            next_a = next_b;
        }
    }

    pub fn tail(&self) -> Option<&Knot> {
        self.0.last()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Knot(i32, i32);

impl Knot {
    pub fn follow(&mut self, other: &Self) {
        let dx = other.0 - self.0;
        let dy = other.1 - self.1;
        let same_row = self.0 == other.0;
        let same_col = self.1 == other.1;

        if (!same_row && !same_col) && (dx.abs() > 1 || dy.abs() > 1) {
            self.0 += dx.signum();
            self.1 += dy.signum();
        } else if dx.abs() > 1 {
            self.0 += dx.signum();
        } else if dy.abs() > 1 {
            self.1 += dy.signum();
        }
    }
}

#[derive(Debug)]
pub struct Direction(i32, i32, i32);

impl FromStr for Direction {
    type Err = String;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        let parts = val.split_whitespace().collect::<Vec<&str>>();

        let [direction, distance, ..] = parts[..] else {
            return Err("Invalid instruction".to_string());
        };

        let distance = distance
            .parse()
            .map_err(|_| "Invalid distance value".to_string())?;

        let result = match direction {
            "U" => Self(1, 0, distance),
            "R" => Self(0, 1, distance),
            "L" => Self(0, -1, distance),
            "D" => Self(-1, 0, distance),
            _ => return Err("Invalid direction value".to_string()),
        };

        Ok(result)
    }
}

impl Direction {
    pub fn apply_to_rope(&self, rope: &mut Rope, tail_visited: &mut Visited) {
        for _ in 0..self.2 {
            rope.move_head((self.0, self.1));
            let tail = rope.tail().unwrap();
            tail_visited.insert(tail.clone());
        }
    }
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Direction>>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let instructions = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();

    Ok(instructions)
}

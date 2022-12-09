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

    pub fn move_head(&mut self, change: (isize, isize)) {
        let mut knots = self.0.iter_mut();

        let mut next_a = knots.next();

        next_a.as_mut().map(|head_knot| {
            head_knot.0 += change.0;
            head_knot.1 += change.1;
        });

        let mut next_b = knots.next();

        loop {
            if next_a.is_some() && next_b.is_some() {
                next_a.as_mut().map(|knot_a| {
                    next_b.as_mut().map(|knot_b| {
                        knot_b.follow(&knot_a);
                    });
                });

                next_a = next_b;
                next_b = knots.next();
            } else {
                break;
            }
        }
    }

    pub fn tail(&self) -> Option<&Knot> {
        self.0.last()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Knot(isize, isize);

impl Knot {
    pub fn follow(&mut self, other: &Self) {
        let dx = other.0 - self.0;
        let dxa = dx.abs();
        let dy = other.1 - self.1;
        let dya = dy.abs();
        let same_row = self.0 == other.0;
        let same_col = self.1 == other.1;

        let x = if dx.is_negative() { -1 } else { 1 };
        let y = if dy.is_negative() { -1 } else { 1 };

        if (!same_row && !same_col) && (dxa > 1 || dya > 1) {
            self.0 += x;
            self.1 += y;
        } else if dxa > 1 {
            self.0 += x;
        } else if dya > 1 {
            self.1 += y;
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    Up(isize),
    Right(isize),
    Left(isize),
    Down(isize),
}

impl FromStr for Instruction {
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
            "U" => Self::Up(distance),
            "R" => Self::Right(distance),
            "L" => Self::Left(distance),
            "D" => Self::Down(distance),
            _ => return Err("Invalid direction value".to_string()),
        };

        Ok(result)
    }
}

impl Instruction {
    pub fn apply_to_rope(&self, rope: &mut Rope, tail_visited: &mut Visited) {
        let (change, amount) = match *self {
            Instruction::Up(dist) => ((1, 0), dist),
            Instruction::Right(dist) => ((0, 1), dist),
            Instruction::Left(dist) => ((0, -1), dist),
            Instruction::Down(dist) => ((-1, 0), dist),
        };

        for _ in 0..amount {
            let old_tail = rope.tail().unwrap().clone();

            rope.move_head(change);

            let new_tail = rope.tail().unwrap();

            if old_tail != *new_tail {
                tail_visited.insert(new_tail.clone());
            }
        }
    }
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Instruction>>
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

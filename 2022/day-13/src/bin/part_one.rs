use std::{env, io};

use aoc_2022_13::{parse_input, Pair, Segment};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let pairs = parse_input(path)?;
    let result = pairs
        .iter()
        .enumerate()
        .filter_map(|(i, pair)| match is_in_right_order(&pair) {
            Ordered::Yes => Some(i + 1),
            _ => None,
        })
        .sum::<usize>();

    Ok(result as u32)
}

#[derive(Debug)]
enum Ordered {
    Undecided,
    Yes,
    No,
}

fn is_in_right_order((left, right): &Pair) -> Ordered {
    test_segment(left, right)
}

fn test_segment(left: &Segment, right: &Segment) -> Ordered {
    match (left, right) {
        (Segment::Num(lval), Segment::Num(rval)) => test_segment_num(*lval, *rval),
        (Segment::List(lval), Segment::List(rval)) => test_segment_list(lval, rval),
        (Segment::List(lval), rval @ Segment::Num(_)) => test_segment_list(lval, &[rval.clone()]),
        (lval @ Segment::Num(_), Segment::List(rval)) => test_segment_list(&[lval.clone()], rval),
    }
}

fn test_segment_list(left: &[Segment], right: &[Segment]) -> Ordered {
    let mut left = left.iter();
    let mut right = right.iter();

    loop {
        match (left.next(), right.next()) {
            (Some(lval), Some(rval)) => match test_segment(lval, rval) {
                Ordered::Undecided => (),
                ord => return ord,
            },
            (None, Some(_)) => return Ordered::Yes,
            (Some(_), None) => return Ordered::No,
            (None, None) => return Ordered::Undecided,
        }
    }
}

fn test_segment_num(left: u32, right: u32) -> Ordered {
    if left < right {
        Ordered::Yes
    } else if left == right {
        Ordered::Undecided
    } else {
        Ordered::No
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(13, result);
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(5938, result);
    }
}

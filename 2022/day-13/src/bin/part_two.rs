use std::{env, io};

use aoc_2022_13::{parse_input, Segment};

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u32> {
    let pairs = parse_input(path)?;
    let mut packets = pairs.iter().fold(vec![], |mut acc, (a, b)| {
        acc.extend([a, b]);
        acc
    });
    let divider_a = Segment::List(vec![Segment::List(vec![Segment::Num(2)])]);
    let divider_b = Segment::List(vec![Segment::List(vec![Segment::Num(6)])]);
    packets.push(&divider_a);
    packets.push(&divider_b);

    packets.sort();

    let result = packets
        .iter()
        .enumerate()
        .filter_map(|(idx, &val)| {
            if val == &divider_a || val == &divider_b {
                Some(idx + 1)
            } else {
                None
            }
        })
        .product::<usize>();

    Ok(result as u32)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(140, result);
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(29025, result);
    }
}

use std::{
    collections::{BTreeMap, HashMap},
    env, io,
};

use itertools::Itertools;

use aoc_2022_11::parse_input;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<u64> {
    let notes = parse_input(path)?;

    let mut activity_counter = HashMap::<usize, u64>::new();
    let mut state = notes
        .iter()
        .enumerate()
        .map(|(i, v)| (i, v.items.clone()))
        .collect::<BTreeMap<_, _>>();

    for _ in 0..20 {
        for monkey_id in 0..state.len() {
            let items = {
                let state = state.get_mut(&monkey_id).unwrap();
                state.drain(..).collect::<Vec<_>>()
            };

            for item in items {
                activity_counter
                    .entry(monkey_id)
                    .and_modify(|v| *v += 1)
                    .or_insert(1);

                let note = &notes[monkey_id];

                let new_value = note.operation.apply(item) / 3;

                let next_monkey = if new_value % note.test_mod == 0 {
                    note.next_monkey_when_true
                } else {
                    note.next_monkey_when_false
                };

                state
                    .entry(next_monkey)
                    .and_modify(|f| f.push(new_value))
                    .or_insert_with(|| vec![new_value]);
            }
        }
    }

    let most_active_product = activity_counter
        .values()
        .sorted_by(|a, b| b.cmp(a))
        .take(2)
        .copied()
        .reduce(|acc, val| acc * val)
        .unwrap();

    Ok(most_active_product)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(10_605, result)
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(316_888, result);
    }
}

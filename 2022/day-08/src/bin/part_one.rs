use std::{env, io};

use aoc_2022_08::{parse_file, Tree, Trees};

fn solve(path: &str) -> io::Result<usize> {
    let trees = parse_file(path)?;

    let mut count = 0;

    for (x, tree_row) in trees.iter().enumerate() {
        for (y, tree) in tree_row.iter().enumerate() {
            if !is_hidden(&trees, tree, x, y) {
                count += 1;
            }
        }
    }

    Ok(count)
}

fn is_hidden(trees: &Trees, tree: &Tree, x: usize, y: usize) -> bool {
    // Edges are never hidden
    if x == 0 || x == trees.len() - 1 || y == 0 || y == trees[x].len() - 1 {
        return false;
    }

    let above_hidden = trees[..x].iter().any(|t| &t[y] >= tree);
    let below_hidden = trees[x + 1..].iter().any(|t| &t[y] >= tree);
    let left_hidden = trees[x][..y].iter().any(|t| t >= tree);
    let right_hidden = trees[x][y + 1..].iter().any(|t| t >= tree);

    above_hidden && below_hidden && left_hidden && right_hidden
}

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() -> io::Result<()> {
        let result = solve("inputs/test.txt")?;

        assert_eq!(21, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(1679, result);

        Ok(())
    }
}

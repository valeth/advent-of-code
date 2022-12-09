use std::{env, io};

use aoc_2022_08::{parse_file, Tree, Trees};

fn solve(path: &str) -> io::Result<usize> {
    let trees = parse_file(path)?;

    let mut higest_score = 0;

    for (x, tree_row) in trees.iter().enumerate() {
        for (y, tree) in tree_row.iter().enumerate() {
            let score = scenic_score(&trees, tree, x, y);

            if score > higest_score {
                higest_score = score;
            }
        }
    }

    Ok(higest_score)
}

fn distance_to_highest_tree<'a>(
    trees: impl IntoIterator<Item = &'a Tree>,
    origin_height: &Tree,
) -> usize {
    let mut score = 0;

    for distant_height in trees {
        score += 1;
        if distant_height >= origin_height {
            break;
        }
    }

    score
}

fn scenic_score(trees: &Trees, tree: &Tree, x: usize, y: usize) -> usize {
    // Edges always score 0
    if x == 0 || x == trees.len() - 1 || y == 0 || y == trees[x].len() - 1 {
        return 0;
    }

    let score_above = distance_to_highest_tree(trees[..x].iter().rev().map(|t| &t[y]), &tree);
    let score_below = distance_to_highest_tree(trees[x + 1..].iter().map(|t| &t[y]), &tree);
    let score_left = distance_to_highest_tree(trees[x][..y].iter().rev(), &tree);
    let score_right = distance_to_highest_tree(&trees[x][y + 1..], &tree);

    let score = score_above * score_below * score_left * score_right;

    score
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

        assert_eq!(8, result);

        Ok(())
    }

    #[test]
    fn puzzle() -> io::Result<()> {
        let result = solve("inputs/puzzle.txt")?;

        assert_eq!(536625, result);

        Ok(())
    }
}

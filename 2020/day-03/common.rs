use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug)]
pub struct TreeMap {
    pub width: usize,
    pub height: usize,
    pub map: Vec<bool>,
}

impl TreeMap {
    pub fn pos_has_tree(&self, x: usize, y: usize) -> bool {
        let pos = x + (y * self.width);
        self.map[pos]
    }

    pub fn find_in_slope(&self, dx: usize, dy: usize) -> u64 {
        let mut tree_count = 0;
        let mut x = 0;
        let mut y = 0;

        loop {
            y += dy;

            if y >= self.height {
                break;
            }

            x += dx;
            x = if x >= self.width { x - self.width } else { x };

            if self.pos_has_tree(x, y) {
                tree_count += 1;
            };
        }

        tree_count
    }
}

pub fn read_file<P: AsRef<Path>>(path: P) -> io::Result<TreeMap> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let width = content.lines().next().unwrap().len();
    let height = content.lines().count();

    Ok(TreeMap {
        width,
        height,
        map: content
            .chars()
            .filter_map(|ch| match ch {
                '#' => Some(true),
                '.' => Some(false),
                _ => None,
            })
            .collect(),
    })
}

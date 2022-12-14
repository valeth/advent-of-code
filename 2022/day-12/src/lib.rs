use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead, BufReader},
    iter,
    path::Path,
};

use petgraph::prelude::DiGraphMap;

pub type Graph = DiGraphMap<PointWithHeight, ()>;
type PointWithHeight = (i32, i32, i32);

pub struct HeightMap {
    items: HashMap<(i32, i32), i32>,
    width: i32,
    height: i32,
}

impl HeightMap {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
            width: 0,
            height: 0,
        }
    }
}

pub fn parse_input<P>(
    path: P,
) -> io::Result<(
    PointWithHeight,
    PointWithHeight,
    Vec<(PointWithHeight, PointWithHeight)>,
)>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let mut height_map = HeightMap::new();
    let mut start_point = None;
    let mut end_point = None;

    for (line_idx, line) in BufReader::new(file).lines().enumerate() {
        let line = line?;

        for (cell_idx, cell) in line.chars().enumerate() {
            height_map.width = (cell_idx + 1) as i32;

            let point = (line_idx as i32, cell_idx as i32);

            let height = if cell == 'S' {
                start_point = Some((point.0, point.1, 1));
                1
            } else if cell == 'E' {
                end_point = Some((point.0, point.1, 26));
                26
            } else {
                (cell as u8 - 96).into()
            };

            height_map.items.insert(point, height);
        }

        height_map.height += 1
    }

    let edges = build_edges(&height_map);
    let start_point = start_point.expect("no start point");
    let end_point = end_point.expect("no end point");

    Ok((start_point, end_point, edges))
}

fn build_edges(map: &HeightMap) -> Vec<(PointWithHeight, PointWithHeight)> {
    map.items
        .keys()
        .flat_map(|point @ (x, y)| {
            let neighbors =
                [(-1, 0), (0, 1), (1, 0), (0, -1)]
                    .iter()
                    .filter_map(move |(nx, ny)| {
                        let dx = x + nx;
                        let dy = y + ny;

                        let inside_bounds =
                            (0..map.height).contains(&dx) && (0..map.width).contains(&dy);

                        if inside_bounds {
                            Some((dx, dy))
                        } else {
                            None
                        }
                    });

            iter::repeat(point).zip(neighbors).filter_map(|(a, b)| {
                let height_self = map.items[&(a.0, a.1)];
                let height_neighbor = map.items[&(b.0, b.1)];

                if height_self + 1 >= height_neighbor {
                    Some(((a.0, a.1, height_self), (b.0, b.1, height_neighbor)))
                } else {
                    None
                }
            })
        })
        .collect()
}

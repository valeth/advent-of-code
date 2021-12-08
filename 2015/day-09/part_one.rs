mod common;
pub mod graph;
mod utils;

use std::cmp::min;
use std::env;

use crate::common::{Distance, Node, NodeSet, Result};

// Only works with complete graphs (where each node leads to each other node)
fn visit_all_shortest(start_node: &Node) -> Vec<(Node, Distance)> {
    let mut visited = NodeSet::new();
    let mut path = Vec::new();
    let mut current_node: Node = start_node.clone();

    'search: loop {
        let mut edge: Option<(Node, Distance)> = None;

        {
            let node = current_node.borrow();
            for (out_node, out_weight) in &node.outgoing {
                if visited.contains(&out_node) {
                    continue;
                }

                edge = Some(match edge {
                    None => (out_node.clone(), *out_weight),
                    Some((n, w)) => {
                        if out_weight < &w {
                            (out_node.clone(), *out_weight)
                        } else {
                            (n, w)
                        }
                    }
                });
            }
        }

        match edge {
            None => break 'search,
            Some((next_node, weight)) => {
                visited.insert(current_node.clone());
                path.push((next_node.clone(), weight));
                current_node = next_node;
            }
        }
    }

    path
}

fn main() -> Result<()> {
    let path = env::args().nth(1).unwrap();
    let directions = common::parse_file(path)?;

    let mut shortest_distance = None;

    for node in &directions.nodes {
        let path = visit_all_shortest(node);

        if path.is_empty() {
            continue;
        }

        let distance: Distance = path.iter().map(|p| p.1).sum();
        shortest_distance = match shortest_distance {
            None => Some(distance),
            Some(d) => Some(min(distance, d)),
        };
    }

    println!("{}", shortest_distance.unwrap_or_default());

    Ok(())
}

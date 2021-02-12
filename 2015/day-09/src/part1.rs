use std::collections::HashSet;
use crate::{Distance, Graph, Node};

type NodeSet = HashSet<Node>;

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

/// Find shortest path that connects two points and visits all locations.
pub fn solve(directions: &Graph) -> Option<Distance> {
    use std::cmp::min;

    let mut shortest_distance = None;

    for node in &directions.nodes {
        let path = visit_all_shortest(node);

        if path.is_empty() {
            continue;
        }

        let distance = path.iter().map(|p| p.1).sum();
        shortest_distance = match shortest_distance {
            None => Some(distance),
            Some(d) => Some(min(distance, d)),
        };
    }

    shortest_distance
}

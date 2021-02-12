use std::collections::HashSet;
use crate::{Distance, Graph, Node};

type NodeSet = HashSet<Node>;

struct Memo<'a> {
    max_dist: &'a mut Distance,
    prev_dist: Distance,
    visited: &'a mut NodeSet,
    path: &'a mut Vec<(Node, Distance)>,
}

fn depth_first_search(node: &Node, memo: Memo<'_>) {
    memo.visited.insert(node.clone());

    let mut distance = 0;

    for (out_node, out_weight) in &node.borrow().outgoing {
        if !memo.visited.contains(&out_node) {
            memo.path.push((out_node.clone(), *out_weight));

            distance = memo.prev_dist + out_weight;

            depth_first_search(&out_node, Memo {
                max_dist: &mut *memo.max_dist,
                prev_dist: distance,
                visited: &mut *memo.visited,
                path: &mut *memo.path
            });
        }

        if *memo.max_dist < distance {
            *memo.max_dist = distance;
        }

        distance = 0
    }
}

fn visit_all_longest(start_node: &Node) -> Vec<(Node, Distance)> {
    let mut visited = NodeSet::new();
    let mut path = Vec::new();

    depth_first_search(&start_node, Memo {
        max_dist: &mut 0,
        prev_dist: 0,
        visited: &mut visited,
        path: &mut path
    });

    path
}

pub fn solve(directions: &Graph) -> Option<Distance> {
    use std::cmp::max;

    let mut longest_distance = None;

    for node in &directions.nodes {
        let path = visit_all_longest(node);

        if path.is_empty() {
            continue;
        }

        let distance = path.iter().map(|p| p.1).sum();
        longest_distance = match longest_distance {
            None => Some(distance),
            Some(d) => Some(max(distance, d)),
        };
    }

    longest_distance
}

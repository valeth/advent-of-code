use std::collections::HashSet;
use crate::{Distance, Graph, Node};

type NodeSet = HashSet<Node>;

struct Memo<'a> {
    max_dist: &'a mut Distance,
    prev_dist: Distance,
    visited: &'a mut NodeSet,
}

fn depth_first_search(node: &Node, memo: Memo<'_>) {
    memo.visited.insert(node.clone());

    let mut distance = 0;

    for (out_node, out_weight) in &node.borrow().outgoing {
        if !memo.visited.contains(&out_node) {
            distance = memo.prev_dist + out_weight;

            depth_first_search(&out_node, Memo {
                max_dist: &mut *memo.max_dist,
                prev_dist: distance,
                visited: &mut *memo.visited,
            });
        }

        if *memo.max_dist < distance {
            *memo.max_dist = distance;
        }

        distance = 0
    }
}

fn visit_all_longest(start_node: &Node) -> Distance {
    let mut visited = NodeSet::new();

    let mut distance = 0;

    depth_first_search(&start_node, Memo {
        max_dist: &mut distance,
        prev_dist: 0,
        visited: &mut visited,
    });

    distance
}

pub fn solve(directions: &Graph) -> Distance {
    use std::cmp::max;

    let mut longest_distance = 0;

    for node in &directions.nodes {
        let distance = visit_all_longest(node);
        longest_distance = max(distance, longest_distance);
    }

    longest_distance
}

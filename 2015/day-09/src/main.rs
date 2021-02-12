mod graph;
mod utils;
mod part1;
mod part2;

type Graph = graph::Graph<Location, Distance>;
type Node = graph::Node<Location, Distance>;

type Location = String;
type Distance = u64;

const FILE_PATHS: &[&str] = &[
    "inputs/test.txt",
    "inputs/puzzle.txt",
];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    for file_path in FILE_PATHS {
        println!("File: {}", file_path);
        let locations = utils::parse_file(file_path)?;

        let mut directions = Graph::default();

        for location in &locations {
            directions.add_edge(location);
        }

        utils::write_dot(&directions, file_path)?;

        println!("\tPart 1: {:?}", part1::solve(&directions));
        println!("\tPart 2: {:?}", part2::solve(&directions));
    }

    Ok(())
}


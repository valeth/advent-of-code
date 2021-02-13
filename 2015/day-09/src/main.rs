mod graph;
mod utils;
mod part1;
mod part2;

type Graph = graph::Graph<Location, Distance>;
type Node = graph::Node<Location, Distance>;

type Location = String;
type Distance = u64;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use std::env;

    let file_paths = env::args().skip(1).collect::<Vec<_>>();

    for file_path in &file_paths {
        let locations = utils::parse_file(file_path)?;

        let mut directions = Graph::default();

        for location in &locations {
            directions.add_edge(location);
        }

        utils::write_dot(&directions, &file_path)?;

        println!("{} / solution 1 = {:?}", file_path, part1::solve(&directions));
        println!("{} / solution 2 = {:?}", file_path, part2::solve(&directions));
    }

    Ok(())
}


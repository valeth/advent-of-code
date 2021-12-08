mod common;
mod graph;

use std::env;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;

use common::{Graph, Result};

fn generate_dot(graph: &Graph, name: &str) -> Result<String> {
    use std::fmt::Write;

    let mut buf = String::new();

    writeln!(buf, "strict graph {} {{", name)?;
    for node in &graph.nodes {
        for (outgoing, weight) in &node.borrow().outgoing {
            writeln!(
                buf,
                "{} -- {} [ label = \"{}\" ];",
                node.borrow().data,
                outgoing.borrow().data,
                weight
            )?;
        }
    }
    writeln!(buf, "}}")?;

    Ok(buf)
}

fn main() -> Result<()> {
    let path = PathBuf::from(env::args().nth(1).unwrap());
    let graph = common::parse_file(&path)?;

    let mut dot_path = path.clone();
    dot_path.set_extension("dot");
    let graph_name = dot_path.file_stem().unwrap().to_str().unwrap();

    let dot = generate_dot(&graph, &graph_name)?;

    let out_path = dot_path.file_name().unwrap();
    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(out_path)?;

    write!(file, "{}", dot)?;

    Ok(())
}

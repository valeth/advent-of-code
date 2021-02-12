use std::{
    io::{self, BufRead},
    path::{Path, PathBuf},
};
use crate::{Location, Distance, Graph};

pub fn generate_dot(graph: &Graph, name: &str) -> Result<String, Box<dyn std::error::Error>> {
    use std::fmt::Write;

    let mut buf = String::new();

    writeln!(buf, "strict graph {} {{", name)?;
    for node in &graph.nodes {
        for (outgoing, weight) in &node.borrow().outgoing {
            writeln!(buf, "{} -- {} [ label = \"{}\" ];",
                node.borrow().data,
                outgoing.borrow().data,
                weight
            )?;
        }
    }
    writeln!(buf, "}}")?;

    Ok(buf)
}

pub fn write_dot<P>(graph: &Graph, path: P) -> Result<(), Box<dyn std::error::Error>>
    where P: Into<PathBuf>
{
    use std::{fs::OpenOptions, io::Write};

    let mut dot_path = path.into();
    dot_path.set_extension("dot");
    let graph_name = dot_path.file_stem().unwrap().to_str().unwrap();
    let dot = generate_dot(&graph, &graph_name)?;
    let out_path = dot_path.file_name().unwrap();
    let mut file = OpenOptions::new().write(true).truncate(true).create(true).open(out_path)?;

    write!(file, "{}", dot)?;
    Ok(())
}

pub fn parse_file<P>(path: P) -> io::Result<Vec<(Location, Location, Distance)>>
    where P: AsRef<Path>
{
    use std::{fs::File, io::BufReader};

    let file = BufReader::new(File::open(path)?);
    let dests = file.lines()
        .map(|l| {
            let line = l.unwrap();
            let words = line.split(' ').collect::<Vec<_>>();
            (words[0].to_string(), words[2].to_string(), words[4].parse::<Distance>().unwrap())
        })
        .collect();

    Ok(dests)
}

use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

#[derive(Debug)]
#[allow(dead_code)]
pub enum Content {
    File { size: usize, path: String },
    Dir { path: String },
}

enum Command {
    ChangeDirectory(String),
    List,
}

pub type FileList = Vec<Content>;
pub type DirMap<T = FileList> = HashMap<String, T>;

pub fn parse_input<P>(path: P) -> io::Result<DirMap>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut cwd = String::from("");

    let mut paths = DirMap::new();
    let mut cur_content = FileList::new();

    for line in reader.lines() {
        let line = line?;

        let parts: Vec<&str> = line.split_whitespace().collect();

        let cmd = match *parts.first().expect("invalid line") {
            "$" => {
                if !cur_content.is_empty() {
                    paths.insert(cwd.clone(), cur_content.drain(..).collect());
                }
                Some(parse_cmd(&parts[1..]))
            }
            _ => None,
        };

        match cmd {
            Some(Command::ChangeDirectory(cd_path)) => {
                cwd = match (cd_path.as_ref(), cwd.as_ref()) {
                    ("/", "/") => cwd,
                    ("/", _) => "/".into(),
                    ("..", _) => match cwd.rsplit_once("/").unwrap() {
                        ("", _) => "/".into(),
                        (p, _) => p.into(),
                    },
                    (p, "/") => format!("/{p}"),
                    (p, _) => format!("{cwd}/{p}"),
                };
            }
            Some(Command::List) => (),
            _ => {
                let content = parse_content_part(&line);
                cur_content.push(content);
            }
        }
    }

    if !cur_content.is_empty() {
        paths.insert(cwd.clone(), cur_content);
    }

    Ok(paths)
}

pub fn path_sizes(dirs: &DirMap, mut size_map: &mut DirMap<usize>) {
    for (path, contents) in dirs {
        let size = size_at_path(&path, &contents, &dirs, &mut size_map);
        size_map.insert(path.to_owned(), size);
    }
}

fn size_at_path(
    cur_path: &str,
    contents: &[Content],
    dirs: &DirMap,
    size_map: &mut DirMap<usize>,
) -> usize {
    let mut sum = 0;

    for content in contents {
        sum += match content {
            Content::File { size, .. } => *size,
            Content::Dir { path } => {
                let dir_path = join_paths(cur_path, path);
                let dir_content = dirs.get(&dir_path).expect("invalid dir path");
                size_at_path(&dir_path, dir_content, dirs, size_map)
            }
        }
    }

    sum
}

fn join_paths(a: &str, b: &str) -> String {
    match a {
        "/" => format!("/{b}"),
        _ => format!("{a}/{b}"),
    }
}

fn parse_content_part(input: &str) -> Content {
    let parts: Vec<_> = input.split_whitespace().collect();

    match parts[..] {
        ["dir", path, ..] => Content::Dir {
            path: path.to_owned(),
        },
        [size, path, ..] => Content::File {
            size: size.parse().unwrap(),
            path: path.to_owned(),
        },
        _ => panic!("invalid content"),
    }
}

fn parse_cmd(input: &[&str]) -> Command {
    match input[..] {
        ["cd", ch_dir, ..] => Command::ChangeDirectory(ch_dir.to_owned()),
        ["ls", ..] => Command::List,
        _ => panic!("unexpected command line input"),
    }
}

use std::{
    collections::{BTreeMap, VecDeque},
    fs::File,
    io::{self, BufRead, BufReader},
    path::Path,
};

pub type Stack = VecDeque<char>;
pub type Stacks = BTreeMap<usize, Stack>;
pub type Instruction = (usize, usize, usize);
pub type Instructions = Vec<Instruction>;

pub fn parse_file<P>(path: P) -> io::Result<(Stacks, Instructions)>
where
    P: AsRef<Path>,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let mut stacks = Stacks::new();
    let mut instructions = Instructions::new();
    let mut reading_instructions = false;

    for line in reader.lines() {
        let line = line?;

        if line.is_empty() {
            reading_instructions = true;
        } else if reading_instructions {
            let instruction = parse_instruction(&line).expect("invalid instruction");
            instructions.push(instruction);
        } else {
            for (index, item) in line.chars().skip(1).step_by(4).enumerate() {
                if item != ' ' && !item.is_numeric() {
                    stacks
                        .entry(index + 1)
                        .and_modify(|v: &mut VecDeque<char>| v.push_back(item))
                        .or_insert_with(|| {
                            let mut stack = VecDeque::new();
                            stack.push_back(item);
                            stack
                        });
                }
            }
        }
    }

    Ok((stacks, instructions))
}

fn parse_instruction(line: &str) -> Option<Instruction> {
    let parts = line.split_whitespace().collect::<Vec<_>>();

    let amount = parts[1].parse().unwrap();
    let from_idx = parts[3].parse().unwrap();
    let to_idx = parts[5].parse().unwrap();

    Some((amount, from_idx, to_idx))
}

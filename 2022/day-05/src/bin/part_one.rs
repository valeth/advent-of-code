use std::{env, io};

use aoc_2022_05::{parse_file, Instruction, Stack, Stacks};

pub fn move_stack(stacks: &mut Stacks, (amount, from_idx, to_idx): Instruction) {
    let source = stacks.get_mut(&from_idx).unwrap();
    let mut swap = Stack::new();
    for _ in 0..amount {
        swap.push_back(source.pop_front().unwrap());
    }

    let target = stacks.get_mut(&to_idx).unwrap();

    for item in swap {
        target.push_front(item);
    }
}

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let (mut stacks, instructions) = parse_file(infile_path)?;

    for instruction in instructions {
        move_stack(&mut stacks, instruction);
    }

    for (_, stack) in stacks {
        let first = stack.front().unwrap();
        print!("{first}");
    }
    println!();

    Ok(())
}

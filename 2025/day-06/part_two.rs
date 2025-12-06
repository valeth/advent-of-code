mod common;

use std::collections::VecDeque;
use std::{env, fs};
use std::path::Path;

use common::{Result, Problem, Operation};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file not provided");
    let problems = parse(infile)?;
    println!("{}", solve(problems));

    Ok(())
}


fn solve(problems: Vec<Problem>) -> u64 {
    problems
        .into_iter()
        .fold(0, |acc, problem| {
            acc + problem.apply_op()
        })
}


fn parse<P>(path: P) -> Result<Vec<Problem>>
where P: AsRef<Path>
{
    type Map = std::collections::BTreeMap<usize, Vec<u64>>;

    let content = fs::read_to_string(path)?;
    let mut nums = Map::new();
    let mut ops = VecDeque::<Operation>::new();

    for row in content.lines() {
        for (col_idx, ch) in row.chars().enumerate() {
            match ch {
                val if val.is_digit(10) => {
                    let val = val.to_digit(10).unwrap();

                    nums.entry(col_idx).and_modify(|entry: &mut Vec<u64>| {
                        entry.push(val.into());
                    }).or_insert_with(|| {
                        vec![val.into()]
                    });
                }
                '+' => ops.push_back(Operation::Add),
                '*' => ops.push_back(Operation::Mul),
                _ => (),
            }
        }
    }

    let mut problem_nums = Vec::new();
    let mut problems = Vec::new();
    let mut iter = nums.into_iter().peekable();

    while let Some((col_idx, num_vec)) = iter.next() {
        let should_collect = match iter.peek() {
            Some((next_idx, _)) => col_idx.abs_diff(*next_idx) > 1,
            None => true,
        };

        let num = vec_to_num(num_vec);
        problem_nums.push(num);

        if should_collect {
            let problem = Problem {
                nums: problem_nums.drain(..).collect(),
                op: ops.pop_front().expect("num-ops mismatch"),
            };
            problems.push(problem);
        }
    };

    Ok(problems)
}


fn vec_to_num(v: Vec<u64>) -> u64 {
    v.into_iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (idx, val)| {
            acc + (val * 10u64.pow(idx as u32))
        })
}

mod common;

use std::path::Path;
use std::{env, fs};

use common::{Result, Problem, Operation};


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file not provided");
    let problems = parse(infile)?;
    println!("{}", solve(problems));

    Ok(())
}


fn solve(problems: Vec<Problem>) -> u64 {
    let mut results = Vec::new();

    for problem in problems {
        results.push(problem.apply_op());
    }

    results.into_iter().sum()
}


fn parse<P>(path: P) -> Result<Vec<Problem>>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;
    let mut problem_nums = Vec::<Vec<u64>>::new();
    let mut problem_ops = Vec::new();

    for line in content.lines() {
        for (col_idx, val) in line.split_whitespace().enumerate() {
            match val {
                "*" => problem_ops.push(Operation::Mul),
                "+" => problem_ops.push(Operation::Add),
                v => {
                    let val = v.parse::<u64>().expect("invalid number");
                    match problem_nums.get_mut(col_idx) {
                        Some(nums) => nums.push(val),
                        None => {
                            let nums = vec![val];
                            problem_nums.push(nums);
                        }
                    }
                }
            }

        }
    }

    let problems = problem_nums
        .into_iter()
        .enumerate()
        .map(|(idx, nums)| {
            Problem { nums, op: problem_ops[idx] }
        })
        .collect();

    Ok(problems)
}

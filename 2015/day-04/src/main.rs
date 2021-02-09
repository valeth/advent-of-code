mod md5;
mod threadpool;

use threadpool::ThreadPool;

macro_rules! infile {
    [$path:literal] => { ($path, include_str!($path)) }
}

const INPUTS: &[(&str, &str)] = &[
    infile!("../inputs/test.txt"),
    infile!("../inputs/puzzle.txt"),
];

fn extract_result(pool: &ThreadPool<Vec<usize>>) -> Option<usize> {
    let mut smallest = None;
    while let Some(result) = pool.next_result().unwrap() {
        let result = result.into_iter().min();
        if !result.is_none() && (smallest.is_none() || smallest > result) {
            smallest = result;
        }
    }
    smallest
}

fn solve_part_1(pool: &ThreadPool<Vec<usize>>, input: &str) -> usize {
    let mut start = 1;
    let mut tries = 7;

    loop {
        if tries == 0 {
            match extract_result(&pool) {
                None =>{ tries = 7 },
                Some(v) => return v,
            }
        }

        let input = input.to_string();
        pool.add_task(move || {
            (start..).into_iter()
                .take(20_000)
                .filter_map(|i| {
                    let secret = format!("{}{}", input, i);
                    let hexdigest = md5::hexdigest(&secret);
                    if hexdigest.starts_with("00000") { Some(i) } else { None }
                })
                .collect::<Vec<_>>()
            });
        tries -= 1;
        start += 20_000;
    }
}

fn solve_part_2(pool: &ThreadPool<Vec<usize>>, input: &str) -> usize {
    let mut start = 1;
    let mut tries = 8;

    loop {
        if tries == 0 {
            match extract_result(&pool) {
                None =>{ tries = 7 },
                Some(v) => return v,
            }
        }

        let input = input.to_string();
        pool.add_task(move || {
            (start..).into_iter()
                .take(20_000)
                .filter_map(|i| {
                    let secret = format!("{}{}", input, i);
                    let hexdigest = md5::hexdigest(&secret);
                    if hexdigest.starts_with("000000") { Some(i) } else { None }
                })
                .collect::<Vec<_>>()
            });

        tries -= 1;
        start += 20_000;
    }
}

fn main() {
    let pool = ThreadPool::with_threads(8);

    for (path, input) in INPUTS {
        println!("File: {}", path);

        for line in input.split('\n') {
            if line.is_empty() { continue }

            println!("\tInput: {}", line);

            let solution = solve_part_1(&pool, &line);
            println!("\t\tFive leading zeroes: {}", solution);

            let solution = solve_part_2(&pool, &line);
            println!("\t\tSix leading zeroes: {}", solution);
        }
    }
}

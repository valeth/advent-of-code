mod md5;

macro_rules! infile {
    [$path:literal] => { ($path, include_str!($path)) }
}

const INPUTS: &[(&str, &str)] = &[
    infile!("inputs/test.txt"),
    infile!("inputs/puzzle.txt"),
];

use std::sync::{Arc, atomic::{AtomicBool, Ordering}};

fn solve_part_1(input: &str, range: (usize, usize), found: Arc<AtomicBool>) {
    for i in range.0..range.1 {
        if found.load(Ordering::SeqCst) {
            break;
        }
        let secret = format!("{}{}", input, i);
        let hexdigest = md5::hexdigest(&secret);

        if hexdigest.starts_with("00000") {
            found.store(true, Ordering::SeqCst);
            println!("5 zeroes found with hexdigest {} for {}: {}", hexdigest, secret, i);
            break;
        }
    }
}

fn solve_part_2(input: &str, range: (usize, usize), found: Arc<AtomicBool>) {
    for i in range.0..range.1 {
        if found.load(Ordering::SeqCst) {
            return;
        }
        let secret = format!("{}{}", input, i);
        let hexdigest = md5::hexdigest(&secret);

        if hexdigest.starts_with("000000") {
            found.store(true, Ordering::SeqCst);
            println!("6 zeroes found with hexdigest {} for {}: {}", hexdigest, input, i);
            break;
        }
    }
}

fn main() {
    use std::{thread, process};

    let mut threads = Vec::new();

    for (path, input) in INPUTS {
        println!("File: {}", path);

        for line in input.split('\n') {
            if line.is_empty() {
                continue;
            }

            let found_solution_1 = Arc::new(AtomicBool::new(false));
            let found_solution_2 = Arc::new(AtomicBool::new(false));

            let ranges = (0..).into_iter().step_by(100_000).take(15).collect::<Vec<_>>();
            for window in ranges.windows(2) {
                let (from, to) = (window[0], window[1]);
                println!("Searching solutions for {} in {}..{}", line, from, to);

                let found = found_solution_1.clone();
                let t = thread::spawn(move || {
                    solve_part_1(&line, (from, to), found)
                });
                threads.push(t);
            }

            let ranges = (0..).into_iter().step_by(1_000_000).take(15).collect::<Vec<_>>();
            for window in ranges.windows(2) {
                let (from, to) = (window[0], window[1]);
                println!("Searching solution for {} in {}..{}", line, from, to);

                let found = found_solution_2.clone();
                let t = thread::spawn(move || {
                    solve_part_2(&line, (from, to), found)
                });
                threads.push(t);
            }
        }
    }

    for thread in threads {
        if let Err(_) = thread.join() {
            eprintln!("Failed to join thread");
            process::exit(1);
        }
    }
}

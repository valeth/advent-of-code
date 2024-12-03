mod common;

use std::env;

use common::{PuzzleInputs, Report};


fn main() {
    let input_file = env::args().nth(1).unwrap();

    let input = PuzzleInputs::parse(input_file);

    let mut num_safe = 0;

    for report in &input.reports {
        if let Status::Safe = is_report_safe(report) {
            num_safe += 1;
        }
    }

    println!("{num_safe}");
}


#[derive(Debug)]
enum Status {
    Safe,
    Unsafe,
}


fn is_report_safe(report: &Report) -> Status {
    let mut order: i32 = 0;

    for window in report.levels.windows(2) {
        let a = window[0];
        let b = window[1];

        let diff = b.abs_diff(a);

        if diff < 1 || diff > 3 {
            return Status::Unsafe;
        }

        if b > a {
            order += 1;
        } else {
            order -= 1;
        }
    }


    if (order.abs() + 1) != (report.levels.len() as i32) {
        return Status::Unsafe;
    }

    Status::Safe
}

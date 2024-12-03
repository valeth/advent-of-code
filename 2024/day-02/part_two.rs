mod common;

use std::collections::BTreeSet;
use std::env;

use common::{PuzzleInputs, Report};


fn main() {
    let input_file = env::args().nth(1).unwrap();

    let input = PuzzleInputs::parse(input_file);

    let mut num_safe = 0;

    for report in &input.reports {
        match is_report_safe(report) {
            Status::Safe => {
                num_safe += 1;
            }
            Status::Unsafe(errors) => {
                if can_tolerate_error(report, &errors) {
                    num_safe += 1;
                }
            }
        }
    }

    println!("{num_safe}");
}


fn can_tolerate_error(report: &Report, errors: &BTreeSet<usize>) -> bool {
    for error_idx in errors {
        let tmp_report = report.without_index(*error_idx);
        if let Status::Safe = is_report_safe(&tmp_report) {
            return true;
        }
    }

    false
}


#[derive(Debug)]
enum Status {
    Safe,
    Unsafe(BTreeSet<usize>),
}


fn is_report_safe(report: &Report) -> Status {
    let first = report.levels.first().unwrap();
    let last = report.levels.last().unwrap();
    let increasing = first < last;

    let mut errors = BTreeSet::new();

    for (idx, window) in report.levels.windows(2).enumerate() {
        let a = window[0];
        let a_idx = idx;
        let b = window[1];
        let b_idx = idx + 1;

        let diff = b.abs_diff(a);

        if (diff < 1 || diff > 3) || (!increasing && b > a) || (increasing && b < a) {
            errors.insert(a_idx);
            errors.insert(b_idx);
        }
    }

    if errors.is_empty() {
        Status::Safe
    } else {
        Status::Unsafe(errors)
    }
}

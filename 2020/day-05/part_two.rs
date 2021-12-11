mod common;

use std::env;
use std::io;

use common::{calculate_seat_ids, parse_file};

fn main() -> io::Result<()> {
    let path = env::args().skip(1).next().unwrap();

    let boarding_passes = parse_file(path)?;

    let mut seat_ids = calculate_seat_ids(&boarding_passes);
    seat_ids.sort();

    for s in seat_ids.windows(2) {
        if s[1] - s[0] == 2 {
            println!("{}", s[0] + 1);
            break;
        }
    }

    Ok(())
}

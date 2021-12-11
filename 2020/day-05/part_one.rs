mod common;

use std::env;
use std::io;

use common::{calculate_seat_ids, parse_file};

fn main() -> io::Result<()> {
    let path = env::args().skip(1).next().unwrap();

    let boarding_passes = parse_file(path)?;

    let seat_ids = calculate_seat_ids(&boarding_passes);

    let highest_seat_id = seat_ids.iter().max().unwrap();

    println!("{}", highest_seat_id);

    Ok(())
}

mod common;

use std::env;

use common::{read_file, Passport, Result};

fn check_valid(passports: Vec<Passport>) -> u64 {
    let mut num_valid = 0;

    for passport in passports {
        if passport.len() == 8 || (passport.len() == 7 && !passport.contains_key("cid")) {
            num_valid += 1;
        }
    }

    num_valid
}

fn main() -> Result<()> {
    let path = env::args().skip(1).next().unwrap();

    let passports = read_file(path)?;

    let num_valid = check_valid(passports);

    println!("{}", num_valid);

    Ok(())
}

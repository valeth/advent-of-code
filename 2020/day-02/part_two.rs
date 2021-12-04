mod common;

use common::{read_file, Result};

fn main() -> Result<()> {
    let entries = read_file()?;

    let mut valid_passwords = 0;

    for (pos1, pos2, req_char, password) in entries {
        let a = password.chars().nth(pos1 - 1).unwrap() == req_char;
        let b = password.chars().nth(pos2 - 1).unwrap() == req_char;

        if a ^ b {
            valid_passwords += 1;
        }
    }

    println!("{}", valid_passwords);

    Ok(())
}

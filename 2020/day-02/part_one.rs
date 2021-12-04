mod common;

use common::{read_file, Result};

fn main() -> Result<()> {
    let entries = read_file()?;

    let mut valid_passwords = 0;

    'entries: for (min, max, req_char, password) in entries {
        let mut char_counter = 0;

        for character in password.chars() {
            if character == req_char {
                char_counter += 1;
            }

            if char_counter > max {
                continue 'entries;
            }
        }

        if char_counter < min {
            continue;
        }

        valid_passwords += 1;
    }

    println!("{}", valid_passwords);

    Ok(())
}

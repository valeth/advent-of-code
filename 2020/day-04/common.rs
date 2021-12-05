use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub type Passport = HashMap<String, String>;

pub fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<Passport>> {
    let mut file = File::open(path)?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let mut passports = vec![];

    for passport in content.split("\n\n") {
        let passport = passport.trim_end();
        let pairs = passport.split(&['\n', ' ', ':'][..]).collect::<Vec<_>>();

        let mut passport = Passport::new();

        for pair in pairs.chunks_exact(2) {
            passport.insert(pair[0].to_owned(), pair[1].to_owned());
        }

        passports.push(passport);
    }

    Ok(passports)
}

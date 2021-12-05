mod common;

use std::env;

use common::{read_file, Passport, Result};

fn check_valid(passports: Vec<Passport>) -> Result<u64> {
    let mut num_valid = 0;

    for passport in passports {
        let mut valid =
            passport.len() == 8 || (passport.len() == 7 && !passport.contains_key("cid"));

        if !valid {
            continue;
        };

        let birth_year = passport.get("byr").unwrap();
        valid = valid
            && birth_year.matches(|c: char| c.is_digit(10)).count() == 4
            && (1920..=2002_u32).contains(&birth_year.parse()?);

        let issue_year = passport.get("iyr").unwrap();
        valid = valid
            && issue_year.matches(|c: char| c.is_digit(10)).count() == 4
            && (2010..=2020_u32).contains(&issue_year.parse()?);

        let expir_year = passport.get("eyr").unwrap();
        valid = valid
            && expir_year.matches(|c: char| c.is_digit(10)).count() == 4
            && (2020..=2030_u32).contains(&expir_year.parse()?);

        let height = passport.get("hgt").unwrap();
        let height_cm = height.strip_suffix("cm").map(|h| h.parse().unwrap());
        let height_in = height.strip_suffix("in").map(|h| h.parse().unwrap());
        valid = valid
            && (height_cm.map(|h| (150..=193_u32).contains(&h)) == Some(true)
                || height_in.map(|h| (59..=76_u32).contains(&h)) == Some(true));

        let hair_color = passport.get("hcl").unwrap().strip_prefix("#");
        valid = valid
            && hair_color.is_some()
            && hair_color
                .unwrap()
                .matches(|c: char| c.is_digit(16))
                .count()
                == 6;

        let eye_color = passport.get("ecl").unwrap();
        valid = valid
            && ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&eye_color.as_str());

        let pass_id = passport.get("pid").unwrap();
        valid = valid && pass_id.matches(|c: char| c.is_digit(10)).count() == 9;

        if valid {
            num_valid += 1;
        }
    }

    Ok(num_valid)
}

fn main() -> Result<()> {
    let path = env::args().skip(1).next().unwrap();

    let passports = read_file(path)?;

    let num_valid = check_valid(passports)?;

    println!("{}", num_valid);

    Ok(())
}

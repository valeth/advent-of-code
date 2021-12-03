mod common;

use common::{read_file, Result};

fn main() -> Result<()> {
    let entries = read_file()?;

    let mut skip_inner = 0;

    for outer in &entries {
        for inner in entries.iter().skip(skip_inner) {
            if outer + inner == 2020 {
                println!("{}", outer * inner);
                return Ok(());
            }
        }
        skip_inner += 1;
    }

    Ok(())
}

mod common;

use common::{read_file, Result};

fn main() -> Result<()> {
    let entries = read_file()?;

    let mut skip_inner = 0;

    for outer in &entries {
        let mut skip_innermost = 0;

        for inner in entries.iter().skip(skip_inner) {
            for innermost in entries.iter().skip(skip_innermost) {
                if outer + inner + innermost == 2020 {
                    println!("{}", outer * inner * innermost);
                    return Ok(());
                }
            }
            skip_innermost += 1;
        }
        skip_inner += 1;
    }

    Ok(())
}

mod common;

use common::{Result, MIN_NUMBER, MAX_NUMBER, parse_file, apply_instruction};


fn main() -> Result<()> {
    let input_file = std::env::args().nth(1).unwrap();
    let instructions = parse_file(input_file)?;

    let mut position: i32 = 50;
    let mut zeros: u64 = 0;

    for instruction in instructions {
        let (new_pos, _) = apply_instruction::<false>(position, instruction);

        position = new_pos;

        assert!(position <= MAX_NUMBER, "({}, {:?})", position, instruction);
        assert!(position >= MIN_NUMBER, "({}, {:?})", position, instruction);

        if position == 0 {
            zeros += 1;
        }
    }

    println!("{zeros}");

    Ok(())
}

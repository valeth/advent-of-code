use std::{env, io};

use aoc_2022_10::Device;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

fn solve(path: &str) -> io::Result<i64> {
    let mut device = Device::initialize(path)?;

    let mut sig_strength_total: i64 = 0;

    device.for_each_cycle(|cycles_total, reg_x| {
        compute_cycle(reg_x, cycles_total, &mut sig_strength_total);
    });

    Ok(sig_strength_total)
}

fn compute_cycle(reg_x: i64, cycles_total: &mut i64, sig_strength_total: &mut i64) {
    *cycles_total += 1;

    if *cycles_total == 20 || (*cycles_total - 20) % 40 == 0 {
        let sig_strength = *cycles_total * reg_x;
        *sig_strength_total += sig_strength;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(13140, result)
    }

    #[test]
    fn puzzle() {
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(13760, result);
    }
}

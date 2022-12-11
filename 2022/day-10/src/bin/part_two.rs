use std::{env, io};

use aoc_2022_10::Device;

fn main() -> io::Result<()> {
    let infile_path = env::args().nth(1).expect("input file");

    let result = solve(&infile_path)?;

    println!("{result}");

    Ok(())
}

type CrtRow = [char; 40];
type Crt = Vec<CrtRow>;

const PIXEL_LIT: char = '#';
const PIXEL_UNLIT: char = '.';
const DRAW_CYCLE: i64 = 40;

fn solve(path: &str) -> io::Result<String> {
    let mut device = Device::initialize(path)?;

    let mut crt = Crt::new();
    let mut crt_row = [PIXEL_UNLIT; 40];

    device.for_each_cycle(|cycles_total, reg_x| {
        compute_cycle(reg_x, cycles_total, &mut crt, &mut crt_row);
    });

    let lines = crt
        .iter()
        .map(|elem| elem.iter().collect::<String>())
        .reduce(|acc, elem| acc + "\n" + &elem)
        .unwrap();

    Ok(lines)
}

fn compute_cycle(reg_x: i64, cycles_total: &mut i64, crt: &mut Crt, crt_row: &mut CrtRow) {
    let cur_row_pos = *cycles_total % DRAW_CYCLE;
    let sprite_area = (reg_x - 1)..=(reg_x + 1);

    if sprite_area.contains(&cur_row_pos) {
        crt_row[cur_row_pos as usize] = PIXEL_LIT;
    }

    *cycles_total += 1;

    if *cycles_total % DRAW_CYCLE == 0 {
        crt.push(*crt_row);
        *crt_row = [PIXEL_UNLIT; 40];
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sample() {
        const OUTPUT: &str = include_str!("../../outputs/part_two/test.txt");
        let result = solve("inputs/test.txt").unwrap();
        assert_eq!(OUTPUT, &result)
    }

    #[test]
    fn puzzle() {
        const OUTPUT: &str = include_str!("../../outputs/part_two/puzzle.txt");
        let result = solve("inputs/puzzle.txt").unwrap();
        assert_eq!(OUTPUT, result);
    }
}

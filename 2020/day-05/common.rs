use std::fs::read_to_string;
use std::io;
use std::path::Path;

pub type BoardingPass = Vec<char>;
pub type SeatId = u32;

pub fn parse_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<BoardingPass>> {
    let content = read_to_string(path)?;

    Ok(content.lines().map(|line| line.chars().collect()).collect())
}

pub fn calculate_seat_ids(boarding_passes: &[BoardingPass]) -> Vec<SeatId> {
    boarding_passes
        .iter()
        .map(|boarding_pass| {
            let mut row = 0..128;
            let mut col = 0..8;

            for instruction in boarding_pass {
                match instruction {
                    'F' => row.end = row.start + row.len() / 2,
                    'B' => row.start += row.len() / 2,
                    'L' => col.end = col.start + col.len() / 2,
                    'R' => col.start += col.len() / 2,
                    ins => panic!("invalid instruction {}", ins),
                }
            }

            (row.start * 8 + col.start) as u32
        })
        .collect()
}

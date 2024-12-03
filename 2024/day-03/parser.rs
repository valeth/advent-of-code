#![allow(dead_code)]

use std::fs;
use std::path::Path;


#[derive(Debug)]
enum ParserState {
    ParseAny,
    ParseMul(u32),
    ParseDo(u32),
    ParseDont(u32),
    ParserEnableMul(bool),
    ParseNumber(u32, [Vec<u32>; 2]),
}


#[derive(Debug)]
pub struct PuzzleInput {
    pub instructions: Vec<(u64, u64)>,
}

impl PuzzleInput {
    pub fn parse<P>(path: P, use_cond: bool) -> Self
    where
        P: AsRef<Path>,
    {
        let content = fs::read_to_string(path).unwrap();
        let mut instructions = Vec::new();

        let mut parse_mul = true;
        let mut state = ParserState::ParseAny;

        for ch in content.chars() {
            state = match (ch, state) {
                ('m', ParserState::ParseMul(0) | ParserState::ParseAny) if parse_mul => {
                    ParserState::ParseMul(1)
                }
                ('u', ParserState::ParseMul(1)) => ParserState::ParseMul(2),
                ('l', ParserState::ParseMul(2)) => ParserState::ParseMul(3),
                ('(', ParserState::ParseMul(3)) => ParserState::ParseNumber(0, [vec![], vec![]]),
                (')', ParserState::ParseNumber(1, nums)) => {
                    let nums = numbers_from_slice(&nums);
                    instructions.push((nums[0], nums[1]));
                    ParserState::ParseAny
                }
                (',', ParserState::ParseNumber(0, nums)) => ParserState::ParseNumber(1, nums),
                ('0'..='9', ParserState::ParseNumber(n, mut nums)) => {
                    let digit = ch.to_digit(10).unwrap();
                    nums[n as usize].push(digit);
                    ParserState::ParseNumber(n, nums)
                }

                ('d', ParserState::ParseDo(0) | ParserState::ParseAny) if use_cond => {
                    ParserState::ParseDo(1)
                }
                ('o', ParserState::ParseDo(1)) => ParserState::ParseDo(2),
                ('n', ParserState::ParseDo(2)) => ParserState::ParseDont(0),
                ('\'', ParserState::ParseDont(0)) => ParserState::ParseDont(1),
                ('t', ParserState::ParseDont(1)) => ParserState::ParseDont(2),

                ('(', ParserState::ParseDo(2)) => ParserState::ParserEnableMul(true),
                ('(', ParserState::ParseDont(2)) => ParserState::ParserEnableMul(false),
                (')', ParserState::ParserEnableMul(cond)) => {
                    parse_mul = cond;
                    ParserState::ParseAny
                }

                _ => ParserState::ParseAny,
            }
        }

        Self { instructions }
    }
}


fn numbers_from_slice(numbers: &[Vec<u32>]) -> Vec<u64> {
    const RADIX: u64 = 10;

    let mut converted = Vec::new();

    for number in numbers {
        let mut tmp_num = 0;

        for (digits, n) in number.into_iter().rev().enumerate() {
            tmp_num += (*n as u64) * RADIX.pow(digits as u32);
        }

        converted.push(tmp_num);
    }

    converted
}

use std::fs;
use std::path::Path;


#[derive(Debug)]
pub enum Instruction {
    Mul(u64, u64),
}


#[derive(Debug)]
pub enum ParserState {
    ParseAny,
    ParseIdent(Ident),
    ParseNumber(u32, [Vec<u32>; 2]),
}


#[derive(Debug)]
pub enum Ident {
    Mul(u32),
}


#[derive(Debug)]
pub struct PuzzleInput {
    pub instructions: Vec<Instruction>,
}

impl PuzzleInput {
    pub fn parse<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let content = fs::read_to_string(path).unwrap();
        let mut instructions = Vec::new();

        let mut state = ParserState::ParseAny;

        for ch in content.chars() {
            state = match (ch, state) {
                ('m', ParserState::ParseIdent(Ident::Mul(0)) | ParserState::ParseAny) => {
                    ParserState::ParseIdent(Ident::Mul(1))
                }
                ('u', ParserState::ParseIdent(Ident::Mul(1))) => {
                    ParserState::ParseIdent(Ident::Mul(2))
                }
                ('l', ParserState::ParseIdent(Ident::Mul(2))) => {
                    ParserState::ParseIdent(Ident::Mul(3))
                }
                ('(', ParserState::ParseIdent(Ident::Mul(3))) => {
                    ParserState::ParseNumber(0, [vec![], vec![]])
                }
                (',', ParserState::ParseNumber(0, nums)) => ParserState::ParseNumber(1, nums),
                (')', ParserState::ParseNumber(1, nums)) => {
                    let nums = numbers_from_slice(&nums);
                    instructions.push(Instruction::Mul(nums[0], nums[1]));
                    ParserState::ParseAny
                }
                ('0'..='9', ParserState::ParseNumber(n, mut nums)) => {
                    let digit = ch.to_digit(10).unwrap();
                    nums[n as usize].push(digit);
                    ParserState::ParseNumber(n, nums)
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

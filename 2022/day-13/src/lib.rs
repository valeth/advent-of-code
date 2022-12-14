use std::{
    fmt,
    fs::File,
    io::{self, Read},
    path::Path,
};

pub type Pair = (Segment, Segment);

#[derive(Clone)]
pub enum Segment {
    Num(u32),
    List(Vec<Segment>),
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(num) => write!(f, "{:?}", num),
            Self::List(elems) => f.debug_list().entries(elems).finish(),
        }
    }
}

pub fn parse_input<P>(path: P) -> io::Result<Vec<Pair>>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let (_, pairs) = parse::pairs(&buf).expect("parsing failed");

    Ok(pairs)
}

mod parse {
    use super::{Pair, Segment};

    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::{self, complete::newline},
        multi::{separated_list0, separated_list1},
        sequence::{delimited, separated_pair},
        IResult,
    };

    pub(super) fn pairs(input: &str) -> IResult<&str, Vec<Pair>> {
        separated_list1(tag("\n\n"), pair)(input)
    }

    fn pair(input: &str) -> IResult<&str, Pair> {
        separated_pair(segment, newline, segment)(input)
    }

    fn segment(input: &str) -> IResult<&str, Segment> {
        alt((list, num))(input)
    }

    fn list(input: &str) -> IResult<&str, Segment> {
        delimited(tag("["), separated_list0(tag(","), segment), tag("]"))(input)
            .map(|(rem, lst)| (rem, Segment::List(lst)))
    }

    fn num(input: &str) -> IResult<&str, Segment> {
        character::complete::u32(input).map(|(rem, num)| (rem, Segment::Num(num)))
    }
}

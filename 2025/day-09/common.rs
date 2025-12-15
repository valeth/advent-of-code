use std::path::Path;
use std::fs;


pub type Error = Box<dyn std::error::Error>;
pub type Result<T, E = Error> = std::result::Result<T, E>;
pub type ArrayList<T> = Vec<T>;
pub type Tiles = ArrayList<Vec2>;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Vec2 {
    pub x: i64,
    pub y: i64,
}


pub fn parse<P>(path: P) -> Result<Tiles>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;
    let tiles = content.lines().map(|line| {
        let (x, y) = line.split_once(',').unwrap();
        let x = x.parse::<i64>()?;
        let y = y.parse::<i64>()?;
        Ok(Vec2 { x, y })
    }).collect::<Result<Tiles>>()?;

    Ok(tiles)
}


#[allow(unused)]
pub fn write_svg(tiles: &Tiles) -> Result<()> {
    let p1 = &tiles[0];
    let mut path = format!("M {},{}", p1.x, p1.y);

    for i in 1..=tiles.len() {
        let p2 = &tiles[i % tiles.len()];
        path += &format!(" L {},{}", p2.x, p2.y);
    }

    let svg = format!(r##"<svg><path d="{}" /></svg>"##, path);
    fs::write("test.svg", svg)?;

    Ok(())
}

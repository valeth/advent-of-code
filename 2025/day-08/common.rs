use std::path::Path;
use std::fs;


pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;
pub type ArrayList<T> = Vec<T>;
pub type JunctionBoxes = ArrayList<Vec3>;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec3(pub u64, pub u64, pub u64);

impl Vec3 {
    pub fn distance(self, rhs: Self) -> f64 {
        [
            self.0.abs_diff(rhs.0),
            self.1.abs_diff(rhs.1),
            self.2.abs_diff(rhs.2)
        ]
            .into_iter()
            .map(|v| v.pow(2) as f64)
            .sum::<f64>()
            .sqrt()
    }
}


pub fn all_distances(boxes: &JunctionBoxes) -> ArrayList<(usize, usize, f64)> {
    let mut distances = ArrayList::new();

    for (idx1, box1) in boxes.iter().enumerate() {
        let start = idx1 + 1;

        for (idx2, box2) in boxes.iter().skip(start).enumerate().map(|(i, v)| (start+i, v)) {
            let distance = box1.distance(*box2);

            distances.push((idx1, idx2, distance));
        }
    }

    distances.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap());

    distances
}


pub fn parse<P>(path: P) -> Result<JunctionBoxes>
where P: AsRef<Path>
{
    let content = fs::read_to_string(path)?;
    let mut boxes = ArrayList::new();

    for line in content.lines() {
        let parts = line
            .split(',')
            .map(|v| v.parse().expect("invalid number"))
            .collect::<ArrayList<_>>();
        assert_eq!(parts.len(), 3);
        boxes.push(Vec3(parts[0], parts[1], parts[2]))
    }

    Ok(boxes)
}

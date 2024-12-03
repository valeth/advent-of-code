use std::fs;
use std::path::Path;


#[derive(Debug)]
pub struct PuzzleInputs {
    pub reports: Vec<Report>,
}

impl PuzzleInputs {
    pub fn parse<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let content = fs::read_to_string(path).unwrap();

        let mut reports = Vec::new();

        for line in content.lines() {
            let levels = line.split(' ').map(|x| x.parse().unwrap()).collect();
            reports.push(Report { levels });
        }

        Self { reports }
    }
}


#[derive(Debug, Clone)]
pub struct Report {
    pub levels: Vec<i32>,
}

impl Report {
    pub fn without_index(&self, index: usize) -> Self {
        let mut new_report = self.clone();
        new_report.levels.remove(index);
        new_report
    }
}

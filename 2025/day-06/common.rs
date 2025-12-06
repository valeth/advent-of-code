pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;


#[derive(Debug)]
pub struct Problem {
    pub nums: Vec<u64>,
    pub op: Operation,
}

impl Problem {
    pub fn apply_op(self) -> u64 {
        match self.op {
            Operation::Mul => {
                self.nums.into_iter().product()
            }
            Operation::Add => {
                self.nums.into_iter().sum()
            }
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Mul,
    Add,
}


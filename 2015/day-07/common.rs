use std::collections::HashMap;

type Value = u16;
type Ident = String;

#[derive(Debug)]
enum Error {
    VariableNotFound,
    InvalidStack,
}

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
enum Token {
    Value(Value),
    Ident(Ident),
    Op(Op),
}

#[derive(Debug, Clone)]
enum Op {
    Assign,
    And,
    Or,
    Lshift,
    Rshift,
    Not,
}

use std::collections::VecDeque;

#[derive(Default, Debug)]
pub struct Circuit {
    variables: HashMap<Ident, Value>,
    stack: Vec<Token>,
    instructions: VecDeque<Vec<Token>>,
    overrides: HashMap<Ident, Value>,
    instruction_len: usize,
}

impl Circuit {
    pub fn add_instruction(&mut self, input: &str) {
        let tokens = tokenize(input);
        self.instructions.push_front(tokens);
    }

    pub fn execute(&mut self) {
        while let Some(tokens) = self.instructions.pop_front() {
            self.execute_one(tokens);
        }
    }

    fn execute_one(&mut self, tokens: Vec<Token>) {
        use self::Op::*;
        use self::Token::*;

        self.instruction_len = tokens.len();

        for token in tokens.clone() {
            let result = match token {
                Ident(_) | Value(_) => {
                    self.stack.push(token.clone());
                    continue;
                }
                Op(Assign) => self.op_assign(),
                Op(Not) => self.op_not(),
                Op(And) => self.binop(|a, b| a & b),
                Op(Or) => self.binop(|a, b| a | b),
                Op(Lshift) => self.binop(|a, b| a.overflowing_shl(b.into()).0),
                Op(Rshift) => self.binop(|a, b| a.overflowing_shr(b.into()).0),
            };

            match result {
                Err(Error::VariableNotFound) => {
                    self.instructions.push_back(tokens);
                    return;
                }
                Err(e) => panic!("{:?}", e),
                _ => (),
            }
        }
    }

    #[allow(dead_code)]
    pub fn variable_override<N>(&mut self, name: N, value: Value)
    where
        N: Into<Ident>,
    {
        self.overrides.insert(name.into(), value);
    }

    #[allow(dead_code)]
    pub fn variables(&self) -> &HashMap<Ident, Value> {
        &self.variables
    }

    pub fn variable<N>(&self, name: N) -> Option<&Value>
    where
        N: AsRef<str>,
    {
        self.variables.get(name.as_ref())
    }

    fn op_assign(&mut self) -> Result<()> {
        use self::Token::*;

        assert!(self.stack.len() == 2);
        let lhs = self.stack.pop().unwrap();
        let rhs = self.stack.pop().unwrap();

        if let Ident(id) = lhs {
            let replace = if self.instruction_len == 3 {
                self.overrides.get(&id)
            } else {
                None
            };
            match (rhs, replace) {
                (Value(_), Some(val)) => {
                    self.variables.insert(id, *val);
                }
                (Value(val), None) => {
                    self.variables.insert(id, val);
                }
                (Ident(id2), _) => {
                    let tmp = *self
                        .variables
                        .get(&id2)
                        .ok_or_else(|| Error::VariableNotFound)?;
                    self.variables.insert(id, tmp);
                }
                _ => return Err(Error::InvalidStack),
            }
        }

        Ok(())
    }

    fn op_not(&mut self) -> Result<()> {
        use self::Token::*;

        assert!(self.stack.len() == 1);
        let token = self.stack.pop().unwrap();

        match token {
            Value(val) => {
                self.stack.push(Value(!val));
            }
            Ident(id) => {
                let val = self
                    .variables
                    .get(&id)
                    .ok_or_else(|| Error::VariableNotFound)?;
                self.stack.push(Value(!val));
            }
            _ => return Err(Error::InvalidStack),
        }

        Ok(())
    }

    fn binop<F>(&mut self, fun: F) -> Result<()>
    where
        F: Fn(Value, Value) -> Value,
    {
        use self::Token::*;

        assert!(self.stack.len() == 2);
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        match (lhs, rhs) {
            (Ident(id1), Value(val)) => {
                let a = *self
                    .variables
                    .get(&id1)
                    .ok_or_else(|| Error::VariableNotFound)?;
                self.stack.push(Value(fun(a, val)));
            }
            (Ident(id1), Ident(id2)) => {
                let a = *self
                    .variables
                    .get(&id1)
                    .ok_or_else(|| Error::VariableNotFound)?;
                let b = *self
                    .variables
                    .get(&id2)
                    .ok_or_else(|| Error::VariableNotFound)?;
                self.stack.push(Value(fun(a, b)));
            }
            (Value(val), Ident(id2)) => {
                let b = *self
                    .variables
                    .get(&id2)
                    .ok_or_else(|| Error::VariableNotFound)?;
                self.stack.push(Value(fun(val, b)));
            }
            _ => return Err(Error::InvalidStack),
        }

        Ok(())
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut operator = None;

    for word in input.split(' ') {
        let token = match word {
            "->" => Token::Op(Op::Assign),
            "AND" => Token::Op(Op::And),
            "OR" => Token::Op(Op::Or),
            "LSHIFT" => Token::Op(Op::Lshift),
            "RSHIFT" => Token::Op(Op::Rshift),
            "NOT" => Token::Op(Op::Not),
            otherwise => match otherwise.parse::<Value>() {
                Ok(val) => Token::Value(val),
                Err(_) => Token::Ident(word.to_string()),
            },
        };

        match token {
            Token::Op(_) => {
                operator = Some(token);
            }
            _ => {
                tokens.push(token);
                if let Some(op) = operator.take() {
                    tokens.push(op);
                }
            }
        }
    }

    tokens
}

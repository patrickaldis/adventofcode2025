use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)] pub enum Operation {
    Multiply,
    Add,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Calculation {
    pub op: Operation,
    pub ns: Vec<i32>,
}

impl Calculation {
    pub fn new(op: Operation, ns: Vec<i32>) -> Self {
        Self { op, ns }
    }
}

pub fn calculate(c: &Calculation) -> i32 {
    match c.op {
        Operation::Multiply => c.ns.iter().product(),
        Operation::Add => c.ns.iter().sum(),
    }
}

pub fn grand_total(cs: &[Calculation]) -> i32 {
    cs.iter().map(calculate).sum()
}

pub fn transpose<T: Clone>(rows: &[Vec<T>]) -> Vec<Vec<T>> {
    let min_len = rows.iter().map(|r| r.len()).min().unwrap_or(0);
    (0..min_len)
        .map(|i| rows.iter().map(|r| r[i].clone()).collect())
        .collect()
}

fn parse_int_row(line: &str) -> Result<Vec<i32>, String> {
    let mut out = Vec::new();
    for tok in line.split_whitespace() {
        let n: i32 = tok
            .parse()
            .map_err(|_| format!("bad int token {tok:?} in line {line:?}"))?;
        out.push(n);
    }
    Ok(out)
}

fn parse_op_row(line: &str) -> Result<Vec<Operation>, String> {
    let mut out = Vec::new();
    for tok in line.split_whitespace() {
        let op = match tok {
            "+" => Operation::Add,
            "*" => Operation::Multiply,
            _ => return Err(format!("bad operator token {tok:?} in line {line:?}")),
        };
        out.push(op);
    }
    Ok(out)
}

pub fn parse_calculations(input: &str) -> Result<Vec<Calculation>, String> {
    let mut lines: Vec<&str> = input
        .lines()
        .map(|l| l.trim_end())
        .filter(|l| !l.trim().is_empty())
        .collect();

    if lines.is_empty() {
        return Ok(vec![]);
    }

    let op_line = lines.pop().unwrap();
    let ops = parse_op_row(op_line)?;

    let mut num_rows = Vec::new();
    for line in lines {
        num_rows.push(parse_int_row(line)?);
    }

    let cols = transpose(&num_rows);
    if ops.len() != cols.len() {
        return Err(format!(
            "mismatched columns: ops={} but transposed num cols={}",
            ops.len(),
            cols.len()
        ));
    }

    Ok(ops
        .into_iter()
        .zip(cols.into_iter())
        .map(|(op, ns)| Calculation::new(op, ns))
        .collect())
}


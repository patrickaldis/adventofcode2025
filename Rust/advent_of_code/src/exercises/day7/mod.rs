use nom::{
    self, IResult, Parser,
    branch::alt,
    character::complete::{char, newline},
    combinator::value,
    multi::many1,
    sequence::{pair, terminated},
};

pub fn beam_output(beam_pattern: &Vec<bool>, input_pattern: &Vec<bool>) -> (Vec<bool>, u64) {
    let mut output: Vec<bool> = input_pattern.clone();

    let split_indices = beam_pattern
        .iter()
        .enumerate()
        .filter(|(i, b)| **b)
        .map(|(i, b)| i);
    let mut splits: u64 = 0;

    for (split_index) in split_indices {
        if input_pattern[split_index] {
            output[split_index] = false;
            splits += 1;

            // Set left index
            if let Some(x) = split_index.checked_sub(1).and_then(|i| output.get_mut(i)) {
                *x = true;
            }

            // Set right index
            if let Some(x) = split_index.checked_add(1).and_then(|i| output.get_mut(i)) {
                *x = true;
            }
        }
    }
    (output, splits)
}

pub fn run_beam(beam_patterns: &Vec<Vec<bool>>, input_pattern: &Vec<bool>) -> (Vec<bool>, u64) {
    let mut current_input = input_pattern.clone();
    let mut splits: u64 = 0;
    for beam_pattern in beam_patterns {
        let (output, num_splits) = beam_output(beam_pattern, &current_input);
        current_input = output;
        splits += num_splits;
    }

    (current_input, splits)
}

pub fn parse_source_row(input: &str) -> IResult<&str, Vec<bool>> {
    many1(alt((value(false, char('.')), value(true, char('S'))))).parse(input)
}

pub fn parse_splitter_row(input: &str) -> IResult<&str, Vec<bool>> {
    many1(alt((value(false, char('.')), value(true, char('^'))))).parse(input)
}

pub fn parse_blank_row(input: &str) -> IResult<&str, ()> {
    value((), many1(char('.'))).parse(input)
}

pub fn parse_splitter_rows(input: &str) -> IResult<&str, Vec<Vec<bool>>> {
    many1(terminated(
        parse_splitter_row,
        (newline, parse_blank_row, newline),
    ))
    .parse(input)
}

pub fn parse_input(input: &str) -> IResult<&str, (Vec<bool>, Vec<Vec<bool>>)> {
    pair(
        terminated(parse_source_row, (newline, parse_blank_row, newline)),
        parse_splitter_rows,
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn check_single_beam_output() {
        let input: Vec<bool> = vec![false, true, false, false, true];
        let pattern: Vec<bool> = vec![false, true, false, false, true];
        let expected_output: Vec<bool> = vec![true, false, true, true, false];

        assert_eq!(beam_output(&pattern, &input), (expected_output, 2));
    }

    #[test]
    fn check_beam_output() {
        let input: Vec<bool> = vec![false, true, false, false, true];
        let pattern: Vec<Vec<bool>> = vec![
            vec![false, true, false, false, true],
            vec![true, false, false, false, true],
        ];
        let expected_output: Vec<bool> = vec![false, true, true, true, false];

        assert_eq!(run_beam(&pattern, &input), (expected_output, 3));
    }

    #[test]
    fn test_parse() {
        let input = "\
..S..
.....
..^..
.....
";

        let (_rest, got) = parse_input(input).expect("parse should succeed");

        let expected = (
            vec![false, false, true, false, false],
            vec![vec![false, false, true, false, false]],
        );

        assert_eq!(got, expected);
    }
    #[test]
    fn run_problem() {
        let input = 
            fs::read_to_string("src/exercises/day7/input.txt").expect("expected to read file");

        let (_rest, parsed) = parse_input(input.as_str()).expect("parse should succeed");

        assert_eq!(run_beam(&parsed.1, &parsed.0).1, 1535);
    }
}

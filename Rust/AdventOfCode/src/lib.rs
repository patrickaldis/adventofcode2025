use combine::parser::char::{digit, letter, newline};
use combine::stream::{easy, StreamErrorFor};
use combine::{many1, optional, sep_by1, Parser, Stream};
use combine::error::StreamError;

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Move {
    MoveRight(u16),
    MoveLeft(u16),
}

fn integer<Input>() -> impl Parser<Input, Output = i32>
where
    Input: Stream<Token = char>,
{
    many1(digit()).map(|digits: String| digits.parse::<i32>().unwrap())
}

fn move_parser<Input>() -> impl Parser<Input, Output = Move>
where
    Input: Stream<Token = char>,
    Input::Range: PartialEq,
{
    (letter(), integer()).and_then(|(dir, n)| match dir {
        'R' => Ok(Move::MoveRight(n as u16)),
        'L' => Ok(Move::MoveLeft(n as u16)),
        _ => Err(StreamErrorFor::<Input>::expected_static_message(
            "expected 'R' or 'L'",
        )),
    })
}

fn move_lines_parser<Input>() -> impl Parser<Input, Output = Vec<Move>>
where
    Input: Stream<Token = char>,
    Input::Range: PartialEq,
{
    sep_by1(move_parser(), newline()).skip(optional(newline()))
}

pub fn cum_sum_mod(sequence: Vec<Move>) -> Vec<i32> {
    cum_sum(sequence).iter().map(|x| x % 100).collect()
}

pub fn cum_sum(sequence: Vec<Move>) -> Vec<i32> {
    sequence
        .iter()
        .scan(50, |st, x| {
            let delta = match x {
                Move::MoveRight(r) => -(*r as i32),
                Move::MoveLeft(l) => *l as i32,
            };
            *st += delta;
            Some(*st)
        })
        .collect()
}

pub fn count_zeros(sequence: Vec<i32>) -> u16 {
    sequence.iter().filter(|&&x| x == 0).count() as u16
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs::{self, read_to_string};

    #[test]
    fn check_calculate_sum() {
        let test_input: Vec<Move> =
            vec![Move::MoveRight(1), Move::MoveLeft(2), Move::MoveRight(10)];
        assert_eq!(cum_sum(test_input), vec![50, 49, 51, 41]);
    }

    #[test]
    fn check_count_zeros() {
        let test_input: Vec<i32> = vec![1, 0, 0, 1, 2, 3];
        assert_eq!(count_zeros(test_input), 2);
    }

    #[test]
    fn check_parse() {
        let test_input: String= "L1\nR2\n".to_string();
    
        let (moves, _rest) = move_lines_parser().parse(test_input.as_str()).unwrap_or_else(|err| panic!["error occured: {err}"]);

        assert_eq!(
            moves,
            vec![Move::MoveLeft(1), Move::MoveRight(2)]
        )
    }

    // #[test]
    // fn reading_file() {
    //     let contents = fs::read_to_string("src/input.txt").expect("expected to read file");
    //     // panic!("{contents}")
    // }
}

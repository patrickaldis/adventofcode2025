use nom::{
    IResult, Parser,
    character::complete::{line_ending, one_of, satisfy},
    multi::many1,
    sequence::terminated,
};

pub fn parse_full(input: &str) -> IResult<&str, Vec<Vec<u8>>> {
    many1(terminated(
        many1(satisfy(|c| c.is_digit(10)).map(|c| c.to_digit(10).unwrap() as u8)),
        line_ending,
    ))
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_full() {
        let input = "1234\n5678\n";

        println!("{:?}", parse_full(input));

        assert!(parse_full(input).is_ok_and(|x| x.1 == vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8]]))
    }
}

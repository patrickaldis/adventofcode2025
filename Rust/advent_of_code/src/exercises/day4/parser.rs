use nom::{
    IResult, Parser,
    branch::alt,
    character::complete::char,
    character::complete::line_ending,
    combinator::opt,
    combinator::value,
    multi::{many1, separated_list1},
};

fn cell(input: &str) -> IResult<&str, bool> {
    // '.' -> false, '@' -> true
    alt((value(false, char('.')), value(true, char('@')))).parse(input)
}

fn row(input: &str) -> IResult<&str, Vec<bool>> {
    many1(cell).parse(input)
}

pub fn diagram_parser(input: &str) -> IResult<&str, Vec<Vec<bool>>> {
    let (rest, rows) = separated_list1(line_ending, row).parse(input)?;
    let (rest, _) = opt(line_ending).parse(rest)?;
    Ok((rest, rows))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_example() {
        let input = "\
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
";

        let (_rest, got) = diagram_parser(input).expect("parse should succeed");

        let expected = vec![
            vec![false ,false ,true  ,true  ,false ,true  ,true  ,true  ,true  ,false],
            vec![true  ,true  ,true  ,false ,true  ,false ,true  ,false ,true  ,true ],
            vec![true  ,true  ,true  ,true  ,true  ,false ,true  ,false ,true  ,true ],
            vec![true  ,false ,true  ,true  ,true  ,true  ,false ,false ,true  ,false],
            vec![true  ,true  ,false ,true  ,true  ,true  ,true  ,false ,true  ,true ],
            vec![false ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,false ,true ],
            vec![false ,true  ,false ,true  ,false ,true  ,false ,true  ,true  ,true ],
            vec![true  ,false ,true  ,true  ,true  ,false ,true  ,true  ,true  ,true ],
            vec![false ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,false],
            vec![true  ,false ,true  ,false ,true  ,true  ,true  ,false ,true  ,false],
        ];

        assert_eq!(got, expected);
    }
}


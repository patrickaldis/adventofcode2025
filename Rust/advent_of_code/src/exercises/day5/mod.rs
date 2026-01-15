use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, space0},
    combinator::{all_consuming, eof, map, map_res},
    multi::{many0, many1},
    sequence::{separated_pair, terminated},
};

type IngredientID = u64;

struct IngredientRange {
    start_id: IngredientID,
    stop_id: IngredientID
}

fn is_fresh(ingredient_ranges:&Vec<IngredientRange>, id:&IngredientID) -> bool {
    ingredient_ranges.into_iter().any(|IngredientRange {start_id, stop_id}| start_id <= id && id <= stop_id)
}

struct IngredientDB {
    ingredient_ranges: Vec<IngredientRange>,
    ingredients: Vec<IngredientID>
}

fn count_fresh(db: IngredientDB) -> u64 {
    let IngredientDB {ingredient_ranges, ingredients} = db;

    let fresh_ingredients = ingredients.iter().filter(|ingredient| is_fresh(&ingredient_ranges, &ingredient));

    fresh_ingredients.count() as u64
}

// parser
fn parse_u64(input: &str) -> IResult<&str, u64> {
    map_res(digit1, str::parse::<u64>).parse(input)
}

fn eol_or_eof(input: &str) -> IResult<&str, ()> {
    map(alt((line_ending, eof)), |_| ()).parse(input)
}

fn parse_range_line(input: &str) -> IResult<&str, IngredientRange> {
    map(
        terminated(separated_pair(parse_u64, tag("-"), parse_u64), eol_or_eof),
        |(start_id, stop_id)| IngredientRange { start_id, stop_id },
    ).parse(input)
}

fn parse_id_line(input: &str) -> IResult<&str, IngredientID> {
    terminated(parse_u64, eol_or_eof).parse(input)
}

fn blank_lines1(input: &str) -> IResult<&str, ()> {
    map(many1(terminated(space0, line_ending)), |_| ()).parse(input)
}

fn parse_db(input: &str) -> IResult<&str, IngredientDB> {
    map(
        all_consuming((
            many1(parse_range_line),
            blank_lines1,
            many1(parse_id_line),
            many0(line_ending), // tolerate trailing newlines
            space0,             // tolerate trailing spaces
        )),
        |(ingredient_ranges, _, ingredients, _, _)| IngredientDB {
            ingredient_ranges,
            ingredients,
        },
    ).parse(input)
}
#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn parses_example() {
        let input = "\
123-345
444123-1231455
124351-134135544
134135-12312
11-111

2
3
5
8
12
6
12
5
";

        let (_rest, db) = parse_db(input).expect("parse should succeed");

        assert_eq!(db.ingredients, vec![2, 3, 5, 8, 12, 6, 12, 5]);
    }

    #[test]
    fn test_run_problem() {
        let test_input: String =
            fs::read_to_string("src/exercises/day5/input.txt").expect("expected to read file");

        let (_rest, input) = parse_db(test_input.as_str())
            .unwrap_or_else(|err| panic!["error occured: {err}"]);

        let final_res = count_fresh(input);

        assert_eq!(final_res, 739)
    }
}

mod parser;

use core::panic;
use std::iter;

pub fn get_location(diagram: &Vec<Vec<bool>>) -> i32 {
    let mut count: i32 = 0;
    let mut output: Vec<Vec<bool>> = diagram.iter().map(|row| vec![false; row.len()]).collect();

    for (i, j) in grid_iterator(output.len(), output[0].len()) {
        if diagram[i][j] == true {
            let adjacent_iter = grid_iterator(3, 3).flat_map(|(x, y)| {
                diagram
                    .get((i + x).wrapping_add_signed(-1))
                    .and_then(|row| row.get((j + y).wrapping_add_signed(-1)))
            });

            let adjacent: Vec<&bool> = adjacent_iter.collect();

            if adjacent.iter().filter(|x| ***x).count() < 5 {
                count += 1;
            }
        }
    }
    count
}

pub fn grid_iterator(n: usize, m: usize) -> impl Iterator<Item = (usize, usize)> {
    (0..m).flat_map(move |i| (0..n).map(move |j| (i, j)))
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::fs;

    use crate::exercises::day4::{get_location, parser::diagram_parser};

    #[test]
    #[rustfmt::skip]
    fn test_get_location() {
        let test_input =
            vec![
                vec![false ,false ,true  ,true  ,false ,true  ,true  ,true  ,true  ,false],
                vec![true  ,true  ,true  ,false ,true  ,false ,true  ,false ,true  ,true ],
                vec![true  ,true  ,true  ,true  ,true  ,false ,true  ,false ,true  ,true ],
                vec![true  ,false ,true  ,true  ,true  ,true  ,false ,false ,true  ,false],
                vec![true  ,true  ,false ,true  ,true  ,true  ,true  ,false ,true  ,true ],
                vec![false ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,false ,true ],
                vec![false ,true  ,false ,true  ,false ,true  ,false ,true  ,true  ,true ],
                vec![true  ,false ,true  ,true  ,true  ,false ,true  ,true  ,true  ,true ],
                vec![false ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,true  ,false],
                vec![true  ,false ,true  ,false ,true  ,true  ,true  ,false ,true  ,false]
            ];

        assert_eq!(get_location(&test_input), 13);
    }

    #[test]
    fn test_run_problem() {
        let test_input: String =
            fs::read_to_string("src/exercises/day4/input.txt").expect("expected to read file");

        let (_rest, input) = diagram_parser(test_input.as_str())
            .unwrap_or_else(|err| panic!["error occured: {err}"]);

        println!("{:?}", input);

        let final_res = get_location(&input);

        assert_eq!(final_res, 1626)
    }
}

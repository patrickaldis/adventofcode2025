pub enum Move {
    MoveRight(u16),
    MoveLeft(u16),
}

pub fn calculate_sum(sequence: Vec<Move>) -> Vec<i32> {
    let mut acc: i32 = 50;
    let mut result: Vec<i32> = vec![50];
    for x in sequence.iter() {
        acc += match x {
            Move::MoveRight(r) => -(*r as i32),
            Move::MoveLeft(l) => *l as i32,
        };
        result.push(acc);
    }
    result
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
        assert_eq!(calculate_sum(test_input), vec![50, 49, 51, 41]);
    }

    #[test]
    fn check_count_zeros() {
        let test_input: Vec<i32> = vec![1, 0, 0, 1, 2, 3];
        assert_eq!(count_zeros(test_input), 2);
    }

    #[test]
    fn reading_file() {
        let contents = fs::read_to_string("src/input.txt").expect("expected to read file");
        // panic!("{contents}")
    }
}

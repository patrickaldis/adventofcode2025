enum Move {
    MoveRight(u16),
    MoveLeft(u16)
}

pub fn calculate_sum(sequence: Vec<Move>) -> Vec<u16> {
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs::{self, read_to_string};

    #[test]
    fn reading_file() {
        let contents = fs::read_to_string("src/input.txt").expect("expected to read file");
        panic!("{contents}")
    }
}

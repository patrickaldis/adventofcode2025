use nom;

pub fn beam_output(beam_pattern:&Vec<bool>, input_pattern:&Vec<bool>) -> (Vec<bool>, u64) {
    let mut output: Vec<bool> = input_pattern.clone();

    let split_indices = beam_pattern.iter().enumerate().filter(|(i, b)| **b).map(|(i, b)| i) ;
    let mut splits:u64 = 0;

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

pub fn run_beam(beam_patterns:&Vec<Vec<bool>>, input_pattern:&Vec<bool>) -> (Vec<bool>, u64) {
    let mut current_input = input_pattern.clone();
    let mut splits:u64 = 0;
    for beam_pattern in beam_patterns {
        let (output , num_splits) = beam_output(beam_pattern, &current_input);
        current_input = output;
        splits += num_splits;
    }

    (current_input, splits)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_single_beam_output() {
        let input:Vec<bool> = vec![false, true, false, false, true];
        let pattern:Vec<bool> = vec![false, true, false, false, true];
        let expected_output:Vec<bool> = vec![true, false, true, true, false];

        assert_eq!(beam_output(&pattern, &input), (expected_output, 2));
    }

    #[test]
    fn check_beam_output() {
        let input:Vec<bool> = vec![false, true, false, false, true];
        let pattern:Vec<Vec<bool>> = vec![
            vec![false, true, false, false, true],
            vec![true, false, false, false, true],
        ];
        let expected_output:Vec<bool> = vec![false, true, true, true, false];

        assert_eq!(run_beam(&pattern, &input), (expected_output, 3));
    }
}

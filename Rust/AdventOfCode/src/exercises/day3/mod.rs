pub fn head_and_tails<T: Clone>(xs: &[T]) -> Vec<(T, Vec<T>)> {
    match xs {
        [x, y] => vec![(x.clone(), vec![y.clone()])],
        [n, rest @ ..] if rest.len() >= 1 => {
            let mut out = Vec::with_capacity(xs.len().saturating_sub(1));
            out.push((n.clone(), rest.to_vec()));
            out.extend(head_and_tails(rest));
            out
        }
        _ => vec![],
    }
}

pub fn find_max_jolt(xs: &[i32]) -> Option<i32> {
    let mut pairs = head_and_tails(xs);
    if pairs.is_empty() {
        return None;
    }

    pairs.reverse();
    let (x, tail) = pairs.into_iter().max_by_key(|(h, _)| *h)?;

    let max_tail = tail.into_iter().max()?;
    Some(10 * x + max_tail)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_head_and_tails() {
        let xs = vec![1, 2, 3, 4];
        assert_eq!(head_and_tails(&xs), vec![(1, vec![2, 3, 4]), (2, vec![3, 4]), (3, vec![4])])
    }

    #[test]
    fn test_find_max_jolt_example() {
        let xs = vec![1, 2, 3, 4];
        assert_eq!(find_max_jolt(&xs), Some(34)); // 10*3 + max([4])

        let xs = vec![1, 2];
        assert_eq!(find_max_jolt(&xs), Some(12)); // 10*3 + max([4])
                                                  
        let xs = vec![9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1];
        assert_eq!(find_max_jolt(&xs), Some(98)); // 10*3 + max([4])

        let xs = vec![8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9];
        assert_eq!(find_max_jolt(&xs), Some(89)); // 10*3 + max([4])

        let xs = vec![2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8];
        assert_eq!(find_max_jolt(&xs), Some(78)); // 10*3 + max([4])

        let xs = vec![8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1];
        assert_eq!(find_max_jolt(&xs), Some(92)); // 10*3 + max([4])
    }
}

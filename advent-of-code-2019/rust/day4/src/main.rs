use std::collections::HashSet;

fn check_4b(n: u32) -> bool {
    let mut acc = n / 10;
    let mut d = n % 10;
    let mut following = 0;
    let mut consequent = false;
    let mut never_decreasing = true;

    while acc > 0 {
        let e = acc % 10;
        never_decreasing &= e <= d;
        if d == e {
            following += 1
        } else {
            if following == 1 {
                consequent = true;
            };
            following = 0;
        };
        d = e;
        acc = acc / 10;
    }
    consequent |= following == 1;
    return consequent && never_decreasing;
}

fn check_4a(n: u32) -> bool {
    let mut acc = n / 10;
    let mut d = n % 10;
    let mut consequent = false;
    let mut never_decreasing = true;
    while acc > 0 {
        let e = acc % 10;
        never_decreasing &= e <= d;
        consequent |= e == d;
        d = e;
        acc = acc / 10;
    }
    return consequent && never_decreasing;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case_4b_4() {
        assert_eq!(check_4b(114444), true);
    }

    #[test]
    fn test_case_4b_2() {
        assert_eq!(check_4b(123444), false);
    }

    #[test]
    fn test_case_4b_3() {
        assert_eq!(check_4b(111122), true);
    }

    #[test]
    fn test_case_4a_1() {
        assert_eq!(check_4a(111111), true);
    }

    #[test]
    fn test_case_4a_2() {
        assert_eq!(check_4a(223450), false);
    }

    #[test]
    fn test_case_4a_3() {
        assert_eq!(check_4a(123789), false);
    }

    #[test]
    fn test_case_4a_4() {
        assert_eq!(check_4a(122345), true);
    }
}

fn compute_4a() -> usize {
    let mut candidates = HashSet::new();
    for n in 382345..=843167 {
        if check_4a(n) {
            candidates.insert(n);
        }
    }
    return candidates.len();
}

fn compute_4b() -> usize {
    let mut candidates = HashSet::new();
    for n in 382345..=843167 {
        if check_4b(n) {
            candidates.insert(n);
        }
    }
    return candidates.len();
}

fn main() {
    println!("Answer 4a: {:?}", compute_4a());
    println!("Answer 4a: {:?}", compute_4b());
}

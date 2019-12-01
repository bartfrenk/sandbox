use std::fs;
use std::io::BufRead;
use std::io::BufReader;

fn per_module_1a(n: u32) -> u32 {
    return n / 3 - 2;
}

fn per_module_1b(n: u32) -> u32 {
    let mut needs = n;
    let mut fuel = 0;
    while needs > 5 {
        needs = needs / 3 - 2;
        fuel += needs;
    }
    return fuel;
}

fn compute_1a() -> u32 {
    let file = fs::File::open("src/input.txt").expect("Couldn't read file");
    let b = BufReader::new(file);
    let mut total = 0;
    for s in b.lines() {
        total += per_module_1a(s.unwrap().parse().unwrap());
    }
    return total;
}

fn compute_1b() -> u32 {
    let file = fs::File::open("src/input.txt").expect("Couldn't read file");
    let b = BufReader::new(file);
    let mut total = 0;
    for s in b.lines() {
        total += per_module_1b(s.unwrap().parse().unwrap());
    }
    return total;
}

fn main() {
    println!("Answer 1a: {}", compute_1a());
    println!("Answer 1b: {}", compute_1b());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case_1b_1() {
        assert_eq!(2, per_module_1b(14));
    }

    #[test]
    fn test_case_1b_2() {
        assert_eq!(966, per_module_1b(1969));
    }

    #[test]
    fn test_case_1b_3() {
        assert_eq!(50346, per_module_1b(100756));
    }
}

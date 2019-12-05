use std::fs;

fn read_input() -> Vec<usize> {
    let s = fs::read_to_string("input.txt").expect("Couldn't read input");
    return s
        .split(",")
        .map(|s| s.trim())
        .map(|s| s.parse().unwrap())
        .collect();
}

fn run_program(intcode: &mut Vec<usize>) -> usize {
    let mut ip = 0;
    while intcode[ip] != 99 {
        let x = intcode[ip + 1];
        let y = intcode[ip + 2];
        let p = intcode[ip + 3];
        match intcode[ip] {
            1 => intcode[p] = intcode[x] + intcode[y],
            2 => intcode[p] = intcode[x] * intcode[y],
            _ => panic!("Invalid opcode: ip={:?}", ip),
        };
        ip += 4;
    }
    return intcode[0];
}

fn compute_2a() -> usize {
    let mut intcode = read_input();
    intcode[1] = 12;
    intcode[2] = 2;
    return run_program(&mut intcode);
}

fn compute_2b() -> Option<usize> {
    let intcode = read_input();
    for noun in 0..100 {
        for verb in 0..100 {
            let mut sample = intcode.clone();
            sample[1] = noun;
            sample[2] = verb;
            if run_program(&mut sample) == 19690720 {
                return Some(100 * noun + verb);
            }
        }
    }
    return None;
}

fn main() {
    println!("Answer 2a: {}", compute_2a());
    println!("Answer 2b: {:?}", compute_2b());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case_2a_1() {
        let mut intcode = vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];
        assert_eq!(3500, run_program(&mut intcode))
    }

    #[test]
    fn test_case_2a_2() {
        let mut intcode = vec![1, 0, 0, 0, 99];
        assert_eq!(2, run_program(&mut intcode))
    }

    #[test]
    fn test_case_2a_3() {
        let mut intcode = vec![2, 4, 4, 5, 99, 0];
        run_program(&mut intcode);
        assert_eq!(9801, intcode[5]);
    }

    #[test]
    fn test_case_2a_4() {
        let mut intcode = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];
        assert_eq!(30, run_program(&mut intcode))
    }
}

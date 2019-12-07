extern crate queues;

use queues::Queue;
use queues::*;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fs;

fn read_input() -> Vec<i32> {
    let s = fs::read_to_string("input.txt").expect("Couldn't read input");
    return s
        .split(",")
        .map(|s| s.trim())
        .map(|s| s.parse().unwrap())
        .collect();
}

fn read_param(intcode: &Vec<i32>, modes: i32, base: usize, n: usize) -> i32 {
    if (modes / 10_i32.pow((n - 1).try_into().unwrap())) % 10 != 0 {
        return intcode[base + n];
    } else {
        return intcode[usize::try_from(intcode[base + n]).unwrap()];
    }
}

fn run_program(intcode: &mut Vec<i32>, inputs: &mut Queue<i32>) -> Vec<i32> {
    let mut outputs = vec![];
    let mut ip = 0;
    let mut opcode = intcode[ip] % 100;
    let mut modes = intcode[ip] / 100;
    while opcode != 99 {
        match opcode {
            1 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                let z = usize::try_from(intcode[ip + 3]).unwrap();
                intcode[z] = x + y;
                ip += 4;
            }
            2 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                let z = usize::try_from(intcode[ip + 3]).unwrap();
                intcode[z] = x * y;
                ip += 4;
            }
            3 => {
                let x = usize::try_from(intcode[ip + 1]).unwrap();
                let i = inputs.remove().unwrap();
                intcode[x] = i;
                ip += 2;
            }
            4 => {
                let x = usize::try_from(intcode[ip + 1]).unwrap();
                outputs.push(intcode[x]);
                ip += 2;
            }
            5 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                if x != 0 {
                    ip = usize::try_from(y).unwrap();
                } else {
                    ip += 3;
                }
            }
            6 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                if x == 0 {
                    ip = usize::try_from(y).unwrap();
                } else {
                    ip += 3;
                }
            }
            7 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                let z = usize::try_from(intcode[ip + 3]).unwrap();
                intcode[usize::try_from(z).unwrap()] = if x < y { 1 } else { 0 };
                ip += 4;
            }
            8 => {
                let x = read_param(intcode, modes, ip, 1);
                let y = read_param(intcode, modes, ip, 2);
                let z = usize::try_from(intcode[ip + 3]).unwrap();
                intcode[usize::try_from(z).unwrap()] = if x == y { 1 } else { 0 };
                ip += 4;
            }
            _ => panic!("Invalid opcode {:?} at ip={:?}", intcode[ip], ip),
        }
        opcode = intcode[ip] % 100;
        modes = intcode[ip] / 100;
    }
    return outputs;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_program_1() {
        let mut intcode = vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        let out = run_program(&mut intcode, &mut queue![8]);
        assert_eq!(vec![1], out);
    }

    #[test]
    fn test_case_5a_1() {
        let out = run_program(&mut vec![1002, 4, 3, 4, 33], &mut queue![]);
        assert_eq!(0, out.len());
    }

    #[test]
    fn test_opcode_3() {
        let mut intcode = vec![3, 1, 99];
        run_program(&mut intcode, &mut queue![2]);
        assert_eq!(vec![3, 2, 99], intcode);
    }

    #[test]
    fn test_opcode_4() {
        let out = run_program(&mut vec![4, 1, 99], &mut queue![]);
        assert_eq!(vec![1], out);
    }

    #[test]
    fn test_answer_5a() {
        assert_eq!(7157989, compute_5a())
    }

    #[test]
    fn test_answer_5b() {
        assert_eq!(7873292, compute_5b())
    }
}

fn compute_5a() -> i32 {
    let mut intcode = read_input();
    return run_program(&mut intcode, &mut queue![1])
        .last()
        .cloned()
        .unwrap();
}

fn compute_5b() -> i32 {
    let mut intcode = read_input();
    return run_program(&mut intcode, &mut queue![5])
        .last()
        .cloned()
        .unwrap();
}

fn main() {
    println!("Answer 5a: {:?}", compute_5a());
    println!("Answer 5b: {:?}", compute_5b());
}

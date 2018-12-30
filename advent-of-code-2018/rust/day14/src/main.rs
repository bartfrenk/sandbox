use std::env;

#[derive(Debug)]
struct State {
    score: Vec<u8>,
    elves: [usize; 2],
}

fn digits(n: u8) -> Vec<u8> {
    let mut rem = n;
    let mut ds: Vec<u8> = Vec::new();
    if rem > 0 {
        while rem > 0 {
            ds.push(rem % 10);
            rem = rem / 10;
        }
    } else {
        ds.push(0);
    }
    return ds;
}

fn step_state(state: &mut State) {
    let x = state.score[state.elves[0]];
    let y = state.score[state.elves[1]];
    for d in digits(x + y).iter().rev() {
        state.score.push(*d);
    }
    let n = state.score.len();
    state.elves[0] = (((state.elves[0] as u32) + (x as u32) + 1) % (n as u32)) as usize;
    state.elves[1] = (((state.elves[1] as u32) + (y as u32) + 1) % (n as u32)) as usize;
}


fn exercise14a() {
    let args: Vec<String> = env::args().collect();
    let n: u32 = args[1].parse::<u32>().unwrap();
    let mut state = State {
        score: vec![3, 7],
        elves: [0, 1],
    };
    while state.score.len() <= ((n + 10) as usize) {
        step_state(&mut state);
    }
    println!("{:?}", &state.score[(n as usize)..(n + 10) as usize]);
}

fn matches(state: &State, n: usize) -> bool {
    return n >= 6 && state.score[n - 1] == 1 && state.score[n - 2] == 4 &&
        state.score[n - 3] == 7 && state.score[n - 4] == 0 &&
        state.score[n - 5] == 8 && state.score[n - 6] == 5;
}

fn exercise14b() {
    let mut state = State {
        score: vec![3, 7],
        elves: [0, 1],
    };
    while !matches(&state, state.score.len()) && !matches(&state, state.score.len() - 1) {
        step_state(&mut state);
    }
    let n = state.score.len();
    println!(
        "{:?}, {:?}",
        state.score.len(),
        &state.score[(n - 10 as usize)..n as usize]
    );
}

fn main() {
    exercise14a();
    exercise14b();
}

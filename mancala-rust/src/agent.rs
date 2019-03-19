use std::io::{stdin, stdout, Write};
use std::num::ParseIntError;

use rand::seq::SliceRandom;

use crate::board::Board;
use crate::board::Player;

pub trait Agent {
    fn exec_move(&self, board: &mut Board, player: Player) -> Result<Player, String>;
}

#[derive(Clone, Copy, Debug)]
pub struct Console {}

impl Console {
    pub fn new() -> Console {
        Console {}
    }
}

impl Agent for Console {
    fn exec_move(&self, board: &mut Board, player: Player) -> Result<Player, String> {
        println!("\n{}\n", board);
        print!(
            "Enter move from {:?} ({:?}): ",
            board.valid_moves(player),
            player
        );
        stdout().flush().unwrap();

        let mut input = String::new();
        stdin().read_line(&mut input).map_err(|e| e.to_string())?;

        let mv: usize = input
            .trim()
            .parse()
            .map_err(|e: ParseIntError| e.to_string())?;
        board.sow(player, mv).map_err(|e| String::from(e))
    }
}

pub struct Random {}

impl Random {
    pub fn new() -> Random {
        Random {}
    }
}

impl Agent for Random {
    fn exec_move(&self, board: &mut Board, player: Player) -> Result<Player, String> {
        println!("\n{}\n", board);
        let options = board.valid_moves(player);
        let mv = options.choose(&mut rand::thread_rng()).unwrap();
        println!("Enter move from {:?} ({:?}): {:?}", options, player, mv);
        stdout().flush().unwrap();

        board.sow(player, *mv).map_err(|e| String::from(e))
    }
}

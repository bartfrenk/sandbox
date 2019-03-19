use crate::board;
use std::num::ParseIntError;

use std::io::{stdin, stdout, Write};

pub trait Agent {
    fn exec_move(&self, board: &mut board::Board) -> Result<board::Player, String>;
}

pub struct HumanPlayer {
    player: board::Player,
}

impl Agent for HumanPlayer {
    fn exec_move(&self, board: &mut board::Board) -> Result<board::Player, String> {
        println!("\n{}\n", board);
        print!(
            "Enter move from {:?} ({:?}): ",
            board.valid_moves(self.player),
            self.player
        );
        stdout().flush().unwrap();

        let mut input = String::new();
        stdin().read_line(&mut input).map_err(|e| e.to_string())?;

        let mv: usize = input
            .trim()
            .parse()
            .map_err(|e: ParseIntError| e.to_string())?;
        board.sow(self.player, mv).map_err(|e| String::from(e))
    }
}

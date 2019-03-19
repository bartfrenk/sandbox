use std::io::{stdin, stdout, Write};

use std::num::ParseIntError;

use crate::board::Board;
use crate::board::Player;

pub trait Agent {
    fn exec_move(&self, board: &mut Board, player: Player) -> Result<Player, String>;
}

#[derive(Clone, Copy, Debug)]
pub struct HumanPlayer {}

impl Agent for HumanPlayer {
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

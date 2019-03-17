use std::io::{stdin, stdout, Write};
use std::num::ParseIntError;

use crate::board::Board;
use crate::board::Player;

const BOARD_SIZE: usize = 14;

pub fn run() -> Result<(), String> {
    let mut board = Board::new(BOARD_SIZE)?;
    let mut player = Player::P1;
    let mut turns = 4;

    while turns > 0 {
        match execute_mv(player, &mut board) {
            Err(s) => eprintln!("{}", s),
            Ok(next) => {
                player = next;
                turns -= 1
            }
        }
    }

    Ok(())
}

fn execute_mv(player: Player, board: &mut Board) -> Result<Player, String> {
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

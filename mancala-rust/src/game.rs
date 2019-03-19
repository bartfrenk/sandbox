use crate::agent::Agent;
use crate::agent::HumanPlayer;
use crate::board::Board;
use crate::board::GameResult;
use crate::board::Player;

const BOARD_SIZE: usize = 14;

pub struct Players {
    player1: Box<dyn Agent>,
    player2: Box<dyn Agent>,
}

impl Players {
    pub fn get_agent(&self, player: Player) -> &Box<dyn Agent> {
        match player {
            Player::P1 => &self.player1,
            Player::P2 => &self.player2,
        }
    }
}

pub fn play(players: Players) -> Result<(), String> {
    let mut board = Board::new(BOARD_SIZE)?;
    let mut player = Player::P1;

    loop {
        match players.get_agent(player).exec_move(&mut board, player) {
            Err(s) => eprintln!("{}", s),
            Ok(next) => {
                player = next;
            }
        }
        match board.winner()? {
            Some(GameResult::Winner(player)) => {
                println!("Player {:?} wins", player);
                break;
            }
            Some(GameResult::Draw) => {
                println!("Game finished with a draw");
                break;
            }
            None => (),
        }
    }

    Ok(())
}

pub fn run() -> Result<(), String> {
    let agents = Players {
        player1: Box::new(HumanPlayer {}),
        player2: Box::new(HumanPlayer {}),
    };

    play(agents)
}

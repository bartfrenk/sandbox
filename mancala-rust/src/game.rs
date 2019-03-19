use crate::agent;
use crate::agent::Agent;
use crate::board::Board;
use crate::board::GameResult;
use crate::board::Player;

const BOARD_SIZE: usize = 14;

pub struct AgentMap {
    player1: Box<dyn Agent>,
    player2: Box<dyn Agent>,
}

impl AgentMap {
    pub fn get(&self, player: Player) -> &Box<dyn Agent> {
        match player {
            Player::P1 => &self.player1,
            Player::P2 => &self.player2,
        }
    }
}

pub fn play(agents: AgentMap) -> Result<(), String> {
    let mut board = Board::new(BOARD_SIZE)?;
    let mut player = Player::P1;

    loop {
        match agents.get(player).exec_move(&mut board, player) {
            Err(s) => eprintln!("{}", s),
            Ok(next) => {
                player = next;
            }
        }
        match board.winner()? {
            Some(GameResult::Winner(player)) => {
                println!("Player {:?} wins", player);
                println!("\n{}\n", board);
                break;
            }
            Some(GameResult::Draw) => {
                println!("Game finished with a draw");
                println!("\n{}\n", board);
                break;
            }
            None => (),
        }
    }

    Ok(())
}

pub fn run() -> Result<(), String> {
    let agents = AgentMap {
        player1: Box::new(agent::Random::new()),
        player2: Box::new(agent::Random::new()),
    };

    play(agents)
}

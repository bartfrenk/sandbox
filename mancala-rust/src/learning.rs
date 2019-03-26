// TODO: Need some way to collapse the state space, as it is too big to be
// represented. Rough calculation indicates that the size of the q-values is
// around 10^2 TB.

// TODO: Probably better to implement some other agents first: see the blog post
// mentioned in the README.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::agent::Agent;
use crate::board::Board;
use crate::board::Player;

pub struct QLearner {
    qvalues: RefCell<HashMap<(Board, usize), f64>>,
    alpha: f64,
    gamma: f64,
}

impl QLearner {
    pub fn new(alpha: f64, gamma: f64) -> QLearner {
        QLearner {
            qvalues: RefCell::new(HashMap::new()),
            alpha: alpha,
            gamma: gamma,
        }
    }

    fn select_move(&self, board: &mut Board, player: Player) -> Result<usize, String> {
        Ok(1)
    }
}

impl Agent for QLearner {
    fn exec_move(&self, board: &mut Board, player: Player) -> Result<Player, String> {
        let mv = self.select_move(board, player)?;
        let freeze = board.clone();
        let map = self.qvalues.borrow_mut();

        let next_player = board.sow(player, mv)?;

        Ok(Player::P1)
    }
}

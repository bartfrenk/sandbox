use std::fmt;
use std::option::Option;
use std::vec::Vec;

type Pit = usize;

#[derive(Debug)]
pub struct Board {
    pits: Vec<Pit>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Player {
    P1,
    P2,
}

pub enum GameResult {
    Winner(Player),
    Draw,
}

impl Player {
    fn other(self) -> Player {
        match self {
            Player::P1 => Player::P2,
            Player::P2 => Player::P1,
        }
    }
}

impl Board {
    // Create a new board with `size` number of pits (including stores).
    pub fn new(size: usize) -> Result<Board, &'static str> {
        if size % 2 != 0 {
            return Err("board size should be even");
        };

        let mut board = Board {
            pits: vec![4; size],
        };
        let size = board.pits.len();
        board.pits[Board::store(size, Player::P1)] = 0;
        board.pits[Board::store(size, Player::P2)] = 0;

        return Ok(board);
    }

    // Execute a move for `player` by sowing all the seeds in position `mv`.
    pub fn sow(&mut self, player: Player, mv: usize) -> Result<Player, &'static str> {
        let mut pos = self.convert_move_to_pos(player, mv)?;
        let mut seeds = self.pits[pos];
        if seeds == 0 {
            return Err("invalid move: no seeds");
        }
        self.pits[pos] = 0;

        while seeds > 0 {
            pos = (pos + 1) % self.pits.len();
            if pos != Board::store(self.pits.len(), player.other()) {
                self.pits[pos] += 1;
                seeds -= 1;
            };
        }
        if self.is_on_players_side(player, pos) && self.pits[pos] == 1 {
            self.collect(player, &[pos, self.opposite(pos)])
        }

        if pos == Board::store(self.pits.len(), player) {
            Ok(player)
        } else {
            Ok(player.other())
        }
    }

    pub fn winner(&self) -> Result<Option<GameResult>, &'static str> {
        if self.all_pits_empty(Player::P1)? || self.all_pits_empty(Player::P2)? {
            let size = self.pits.len();
            let seeds_p1 = self.pits[Board::store(size, Player::P1)];
            let seeds_p2 = self.pits[Board::store(size, Player::P2)];
            if seeds_p1 > seeds_p2 {
                return Ok(Some(GameResult::Winner(Player::P1)));
            }
            if seeds_p1 < seeds_p2 {
                return Ok(Some(GameResult::Winner(Player::P2)));
            }
            return Ok(Some(GameResult::Draw));
        }
        Ok(None)
    }

    // Checks whether `player` wins the game.
    fn all_pits_empty(&self, player: Player) -> Result<bool, &'static str> {
        let pits_per_side = self.pits.len() / 2 - 1;
        let pos = self.convert_move_to_pos(player, 1)?;
        for i in pos..pos + pits_per_side {
            if self.pits[i] > 0 {
                return Ok(false);
            }
        }
        return Ok(true);
    }

    fn convert_move_to_pos(&self, player: Player, mv: usize) -> Result<usize, &'static str> {
        if mv < 1 || mv > self.pits.len() / 2 - 1 {
            return Err("invalid move: not on board");
        }
        let store_index = Board::store(self.pits.len(), player);
        let pits_per_side = self.pits.len() / 2 - 1;
        Ok(store_index - (pits_per_side + 1 - mv))
    }

    // Check whether the index `pos` is on the side of `player`.
    fn is_on_players_side(&self, player: Player, pos: usize) -> bool {
        let hi = Board::store(self.pits.len(), player);
        let lo = hi - (self.pits.len() / 2 - 1);
        return lo <= pos && pos < hi;
    }

    // Move the seeds from the pits indexed by `sources` into the store of
    // `player`.
    fn collect(&mut self, player: Player, sources: &[usize]) {
        let mut total = 0;
        for src in sources {
            total += self.pits[*src];
            self.pits[*src] = 0;
        }
        let dest = Board::store(self.pits.len(), player);
        self.pits[dest] += total;
    }

    // Compute the index of the store of `player` given total board size `size`.
    // Making this a method is difficult due to borrowing rules, since it is
    // often used as in index in an expression that mutates the pits vector.
    fn store(size: usize, player: Player) -> usize {
        match player {
            Player::P1 => size / 2 - 1,
            Player::P2 => size - 1,
        }
    }

    // Return the position on the board opposite to `pos`.
    fn opposite(&self, pos: usize) -> usize {
        return self.pits.len() - 2 - pos;
    }

    pub fn valid_moves(&self, player: Player) -> Vec<usize> {
        let mut mvs = vec![];
        let pits_per_side = self.pits.len() / 2 - 1;

        for i in 1..=pits_per_side {
            if self.pits[self.convert_move_to_pos(player, i).unwrap()] > 0 {
                mvs.push(i);
            }
        }
        return mvs;
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let split = Board::store(self.pits.len(), Player::P1);
        for pit in self.pits[split + 1..].iter().rev() {
            write!(f, "{:3}", pit)?;
        }
        write!(f, "\n   ")?;
        for pit in self.pits[..=split].iter() {
            write!(f, "{:3}", pit)?;
        }
        Ok(())
    }
}

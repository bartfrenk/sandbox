use std::fmt;

const BOARD_SIZE: usize = 14;
const STORES: [usize; 2] = [BOARD_SIZE / 2 - 1, BOARD_SIZE - 1];
const SIDE: usize = BOARD_SIZE / 2 - 1;

use crate::utils;

#[derive(Debug)]
struct Board {
    pits: [Pit; BOARD_SIZE],
}

#[derive(Debug, Clone, Copy)]
struct Pit(u8);

impl Pit {
    fn inc(&mut self, count: u8) {
        self.0 += count;
    }

    fn dec(&mut self, count: u8) {
        self.0 -= count;
    }

    fn clear(&mut self) {
        self.0 = 0;
    }

    fn empty(&self) -> bool {
        self.0 == 0
    }

    fn collect(&mut self, sources: &mut [Pit]) {
        for source in sources {
            self.inc(source.0);
            source.clear();
        }
    }

    fn has(&self, count: u8) -> bool {
        return self.0 == count;
    }
}

#[derive(Debug, Clone, Copy)]
enum Player {
    P1,
    P2,
}

fn other(player: Player) -> Player {
    match player {
        Player::P1 => Player::P2,
        Player::P2 => Player::P1,
    }
}

impl fmt::Display for Pit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:2}", self.0)
    }
}

impl Board {
    pub fn new() -> Board {
        let mut board = Board {
            pits: [Pit(4); BOARD_SIZE],
        };
        for i in &STORES {
            board.pits[*i] = Pit(0);
        }
        board.pits[5] = Pit(0);
        return board;
    }

    fn is_on_players_side(&self, player: Player, start: usize) -> bool {
        let hi = self.store(player);
        let lo = hi - SIDE;
        return lo <= start && start < hi;
    }

    fn store(&self, player: Player) -> usize {
        match player {
            Player::P1 => STORES[0],
            Player::P2 => STORES[1],
        }
    }

    fn opposite(&self, pos: usize) -> usize {
        return BOARD_SIZE - 2 - pos;
    }

    // TODO: This could be implemented cleaner using an iterator over the positions.
    pub fn sow(&mut self, player: Player, start: usize) -> Result<Player, &'static str> {
        if !self.is_on_players_side(player, start) {
            return Err("invalid move");
        };
        let mut seeds = self.pits[start];
        self.pits[start].clear();
        let mut pos = start;
        while !seeds.empty() {
            pos = (pos + 1) % BOARD_SIZE;
            if pos != self.store(other(player)) {
                self.pits[pos].inc(1);
                seeds.dec(1);
            };
        }
        println!("{:?}, {:?}", player, pos);
        if self.is_on_players_side(player, pos) && self.pits[pos].has(1) {
            self.pits[self.store(player)]
                .collect(&mut [self.pits[pos], self.pits[self.opposite(pos)]])
        }

        if pos == self.store(player) {
            Ok(player)
        } else {
            Ok(other(player))
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let top = utils::join(self.pits[STORES[0] + 1..].iter().rev(), " ");
        let bottom = utils::join(self.pits[..=STORES[0]].iter(), " ");
        write!(f, "{}\n   {}", top, bottom)
    }
}

pub fn test() {
    let mut board = Board::new();
    board.sow(Player::P1, 1).unwrap();
    println!("{}", board);
}

use std::fmt;
use std::vec::Vec;

const BOARD_SIZE: usize = 14;
const STORES: [usize; 2] = [BOARD_SIZE / 2 - 1, BOARD_SIZE - 1];
const SIDE: usize = BOARD_SIZE / 2 - 1;

#[derive(Debug)]
struct Board {
    pits: Vec<Pit>,
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
    pub fn new(size: usize) -> Board {
        let mut board = Board {
            pits: vec![Pit(4); size],
        };
        for i in &STORES {
            board.pits[*i] = Pit(0);
        }
        return board;
    }

    fn is_on_players_side(&self, player: Player, start: usize) -> bool {
        let hi = self.store(player);
        let lo = hi - SIDE;
        return lo <= start && start < hi;
    }

    fn collect(&mut self, dest: usize, sources: &[usize]) {
        let mut total = 0;
        for src in sources {
            total += self.pits[*src].0;
            self.pits[*src].clear();
        }
        self.pits[dest].inc(total)
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
            self.collect(self.store(player), &[pos, self.opposite(pos)])
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
        for pit in self.pits[STORES[0] + 1..].iter().rev() {
            write!(f, "{:3}", pit.0)?;
        }
        write!(f, "\n   ")?;
        for pit in self.pits[..=STORES[0]].iter() {
            write!(f, "{:3}", pit.0)?;
        }
        Ok(())
    }
}

pub fn test() {
    let mut board = Board::new(BOARD_SIZE);
    board.sow(Player::P1, 1).unwrap();
    println!("{}", board);
}

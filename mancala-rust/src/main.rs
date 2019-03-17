fn main() {
    match mancala::game::run() {
        Ok(()) => return,
        Err(msg) => eprintln!("{}", msg),
    }
}

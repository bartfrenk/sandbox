fn main() {
    match mancala::board::test() {
        Ok(_) => return,
        Err(s) => eprintln!("{}", s)
    };
}

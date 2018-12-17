use std::fs::File;
use std::io::Read;


fn read_input() -> Vec<u8> {
    let mut file = File::open("res/input-13.txt").expect("Unable to open file");
    let mut buffer = vec![];
    file.read_to_end(&mut buffer).expect("Error reading file");
    let it = buffer.split(|x| *x == 10);
    for line in it {}
}


fn main() {
    println!("{:?}", read_input());
}

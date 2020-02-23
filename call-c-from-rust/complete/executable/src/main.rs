extern crate libc;

extern "C" {
    fn from_the_library(a: u8, b: u8) -> u8;
}

fn main() {
    unsafe {
        println!("Adding: {}", from_the_library(1, 2));
    }
}

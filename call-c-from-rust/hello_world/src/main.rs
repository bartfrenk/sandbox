extern "C" {
    pub fn strcpy(dest: *mut u8, src: *const u8) -> *mut u8;
    pub fn puts(s: *const u8) -> i32;
}

fn main() {
    let x = b"Hello world!\0";
    let mut y = [0u8; 32];
    unsafe {
        strcpy(y.as_mut_ptr(), x.as_ptr());
        puts(y.as_ptr());
    }
}

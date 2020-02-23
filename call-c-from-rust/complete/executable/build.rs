fn main() {
    println!("cargo:rustc-link-lib=dylib=awesome_math");
    println!("cargo:rustc-link-search=native=../library");
}

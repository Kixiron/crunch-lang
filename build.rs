fn main() {
    println!("cargo:rerun-if-changed=benches/factorial.c");
    println!("cargo:rerun-if-changed=benches/fibonacci.c");

    cc::Build::new()
        .file("./benches/factorial.c")
        .compile("libfactorial.a");

    cc::Build::new()
        .file("./benches/fibonacci.c")
        .compile("libfibonacci.a");
}

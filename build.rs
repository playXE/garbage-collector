fn main() {
    cc::Build::new()
        //.flag("-nostdinc")
        //.flag("-nostdlib")
        //.flag("-fno-stack-protector")
        //.flag("-Wno-expansion-to-defined")
        .file("deps/dlmalloc.c")
        .flag("-O3")
        .compile("dlmalloc_c");

    println!("cargo:rustc-link-lib=dlmalloc_c");
}

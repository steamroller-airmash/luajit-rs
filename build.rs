extern crate cmake;

pub fn main() {
  let path = cmake::build("luajit");

  println!("cargo:rustc-link-lib=luajit");
  println!("cargo:rustc-link-search=native={}/lib", path.display());
  println!("cargo:rerun-if-changed=build.rs");
}

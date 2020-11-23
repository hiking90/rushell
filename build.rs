extern crate lalrpop;

fn main() {
    // lalrpop::Configuration::new()
    //     .generate_in_source_tree()
    //     .process().unwrap();
   lalrpop::process_root().unwrap();
}

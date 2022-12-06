use itertools::Itertools;
use std;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let count: usize = args[1].parse().unwrap();
    let input = args[2].as_bytes();

    let result = input
        .windows(count)
        .position(|w| w.iter().unique().count() == count)
        .unwrap();

    println!("{:?}", result + count);
}

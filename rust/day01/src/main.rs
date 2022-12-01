use itertools::Itertools;
use std::fs::read_to_string;

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let elves = input
        .trim_end()
        .split("\n\n")
        .map(|s| s.split("\n").map(|x| x.parse::<i32>().unwrap()));

    let sums: Vec<i32> = elves.map(|elf| elf.sum()).sorted().rev().collect();
    println!("{}", sums[0]);
    println!("{}", sums[0] + sums[1] + sums[2]);
}

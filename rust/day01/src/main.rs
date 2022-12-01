use std::fs::read_to_string;

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let elves = input
        .trim_end()
        .split("\n\n")
        .map(|s| s.split("\n").map(|x| x.parse::<i32>().unwrap()));

    let mut sums: Vec<i32> = elves.map(|elf| elf.sum()).collect();
    sums.sort_by(|a, b| b.cmp(a));
    println!("{}", sums[0]);
    println!("{}", sums[0] + sums[1] + sums[2]);
}

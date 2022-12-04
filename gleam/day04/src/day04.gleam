import gleam/erlang/file
import gleam/io
import gleam/int
import gleam/list
import gleam/option
import gleam/regex
import gleam/string

pub fn main() {
  assert Ok(input) = file.read("../../input04.txt")
  let input =
    input
    |> string.trim()
    |> string.split(on: "\n")
    |> list.map(fn(x) {
      assert Ok(re) = regex.from_string("(\\d+)-(\\d+),(\\d+)-(\\d+)")
      case regex.scan(with: re, content: x) {
        [regex.Match(_, matches)] ->
          matches
          |> list.map(fn(x) {
            assert option.Some(x) = x
            assert Ok(x) = int.parse(x)
            x
          })
      }
    })

  // Look for pairs where one is completely contained in the other.
  let part1 =
    input
    |> list.filter(contains)
    |> list.length()

  io.debug(part1)

  // Look for pairs where one overlaps the other.
  let part2 =
    input
    |> list.filter(overlaps)
    |> list.length()

  io.debug(part2)
}

pub fn contains(x) {
  let [a0, a1, b0, b1] = x
  a0 >= b0 && a1 <= b1 || b0 >= a0 && b1 <= a1
}

pub fn overlaps(x) {
  let [a0, a1, b0, b1] = x
  a1 >= b0 && b1 >= a0
}

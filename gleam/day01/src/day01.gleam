import gleam/erlang/file
import gleam/io
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn main() {
  assert Ok(input) = file.read("input.txt")
  let input =
    input
    |> string.trim()
    |> string.split(on: "\n\n")
    |> list.map(fn(xs) {
      xs
      |> string.split("\n")
      |> list.map(fn(x) {
        assert Ok(n) = int.parse(x)
        n
      })
    })

  let sums =
    input
    |> list.map(fn(xs) {
      list.reduce(xs, fn(acc, x) { acc + x })
      |> result.unwrap(0)
    })
    |> list.sort(fn(a, b) { int.compare(b, a) })

  let [top, ..] = sums
  io.debug(top)

  let [a, b, c, ..] = sums
  io.debug(a + b + c)
}

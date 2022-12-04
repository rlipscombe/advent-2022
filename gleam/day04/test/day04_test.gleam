import gleeunit
import gleeunit/should
import day04

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn contains_test() {
  day04.contains([1, 4, 6, 9])
  |> should.be_false

  day04.contains([1, 7, 6, 9])
  |> should.be_false

  day04.contains([1, 4, 1, 4])
  |> should.be_true

  day04.contains([4, 4, 6, 9])
  |> should.be_false

  day04.contains([7, 8, 6, 9])
  |> should.be_true

  day04.contains([6, 9, 7, 8])
  |> should.be_true

  day04.contains([6, 9, 4, 5])
  |> should.be_false
}

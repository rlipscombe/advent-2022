import gleam/erlang/file
import gleam/io
import gleam/int
import gleam/list
import gleam/result
import gleam/string

type Move {
  Rock
  Paper
  Scissors
}

type Winner {
  Them
  Draw
  Me
}

pub fn main() {
  assert Ok(input) = file.read("../../input02.txt")
  let input =
    input
    |> string.trim()
    |> string.split(on: "\n")

  let rounds =
    input
    |> list.map(fn(in) {
      let [them, me] = string.split(in, " ")
      #(
        case them {
          "A" -> Rock
          "B" -> Paper
          "C" -> Scissors
        },
        case me {
          "X" -> Rock
          "Y" -> Paper
          "Z" -> Scissors
        },
      )
    })

  let result = score_game(rounds)
  io.debug(result)

  let rounds =
    input
    |> list.map(fn(in) {
      let [them, outcome] = string.split(in, " ")

      #(
        case them {
          "A" -> Rock
          "B" -> Paper
          "C" -> Scissors
        },
        case outcome {
          "X" -> Them
          "Y" -> Draw
          "Z" -> Me
        },
      )
    })

  let rounds =
    list.map(
      rounds,
      fn(round) {
        let #(them, outcome) = round
        case outcome {
          Draw -> #(them, them)
          Me -> #(them, beat(them))
          Them -> #(them, cede(them))
        }
      },
    )

  let result = score_game(rounds)
  io.debug(result)
  0
}

fn beat(it) {
  case it {
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock
  }
}

fn cede(it) {
  case it {
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
  }
}

fn score_game(rounds) {
  list.fold(
    rounds,
    0,
    fn(acc, round) {
      acc + case winner(round) {
        Them -> 0
        Draw -> 3
        Me -> 6
      } + case round {
        #(_, Rock) -> 1
        #(_, Paper) -> 2
        #(_, Scissors) -> 3
      }
    },
  )
}

fn winner(round) {
  case round {
    #(Rock, Paper) -> Me
    #(Rock, Scissors) -> Them
    #(Paper, Rock) -> Them
    #(Paper, Scissors) -> Me
    #(Scissors, Rock) -> Me
    #(Scissors, Paper) -> Them
    _ -> Draw
  }
}

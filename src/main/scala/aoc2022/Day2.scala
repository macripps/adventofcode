package aoc2022

import aoc.NewDay

class Day2 extends NewDay(2022, 2) {
  part(1) {
    execute { in =>
      in.map { line =>
        val opponent = line.charAt(0) - 'A';
        val choice = line.charAt(2) - 'X';
        1 + choice + ((4 + choice - opponent) % 3) * 3
      }.sum.toString
    }
  }

  part(2) {
    execute { in =>
      in.map { line =>
        val opponent = line.charAt(0) - 'A';
        val result = line.charAt(2) - 'X';
        1 + ((opponent + 2 + result) % 3) + (result * 3)
      }.sum.toString
    }
  }
}

object Day2Main extends Day2

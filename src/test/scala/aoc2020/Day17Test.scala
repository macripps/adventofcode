package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  test("example part1") {
    val input =
      """.#.
        |..#
        |###""".stripMargin.split("\n")

    assert(Day17().part1(input) === "There are 112 active cells.")
  }

  test("example part2") {
    val input =
      """.#.
        |..#
        |###""".stripMargin.split("\n")

    assert(Day17().part2(input) === "There are 848 active cells.")
  }

}

package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

  test("examples") {
    val grid = Array(
      ".#.#.#".toCharArray,
      "...##.".toCharArray,
      "#....#".toCharArray,
      "..#...".toCharArray,
      "#.#..#".toCharArray,
      "####..".toCharArray
    )
    assert(Day18.neighbours(grid, 0, 0) == 1)
    assert(Day18.neighbours(grid, 0, 1) == 0)
    assert(Day18.neighbours(grid, 1, 1) == 2)
  }
}

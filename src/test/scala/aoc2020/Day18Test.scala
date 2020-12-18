package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

  test("part1 examples") {
    assert(Day18.parse("1 + 2 * 3 + 4 * 5 + 6".replace(" ", "")) === 71)
    assert(Day18.parse("1 + (2 * 3) + (4 * (5 + 6))".replace(" ", "")) === 51)
    assert(Day18.parse("2 * 3 + (4 * 5)".replace(" ", "")) === 26)
    assert(Day18.parse("5 + (8 * 3 + 9 + 3 * 4 * 3)".replace(" ", "")) === 437)
    assert(Day18.parse("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".replace(" ", "")) === 12240)
    assert(Day18.parse("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".replace(" ", "")) === 13632)
  }

  test("part2 examples") {
    assert(Day18.parse2("1 + 2 * 3 + 4 * 5 + 6".replace(" ", "")) === 231)
    assert(Day18.parse2("1 + (2 * 3) + (4 * (5 + 6))".replace(" ", "")) === 51)
    assert(Day18.parse2("2 * 3 + (4 * 5)".replace(" ", "")) === 46)
    assert(Day18.parse2("5 + (8 * 3 + 9 + 3 * 4 * 3)".replace(" ", "")) === 1445)
    assert(Day18.parse2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))".replace(" ", "")) === 669060)
    assert(Day18.parse2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2".replace(" ", "")) === 23340)
  }

}

package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  test("part1 examples") {
    assert(!Day11.valid("hijklmmn"))
    assert(!Day11.valid("abbceffg"))
    assert(!Day11.valid("abbcegjk"))

    assert(Day11.next("abcdefgh") === "abcdffaa")
    assert(Day11.next("ghijklmn") === "ghjaabcc")
  }

}

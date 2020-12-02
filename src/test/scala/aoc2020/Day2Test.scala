package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  test("example passwords are valid") {
    assert(Day2.isValidPart1("abcde", 'a', 1, 3))
    assert(Day2.isValidPart1("ccccccccc", 'c', 2, 9))
    assert(Day2.isValidPart2("abcde", 'a', 1, 3))
  }

  test("example passwords are invalid") {
    assert(!Day2.isValidPart1("cdefg", 'b', 1, 3))
    assert(!Day2.isValidPart2("ccccccccc", 'c', 2, 9))
    assert(!Day2.isValidPart2("cdefg", 'b', 1, 3))
  }
}

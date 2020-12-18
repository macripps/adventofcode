package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {

  test("part 1 examples") {
    assert(Day8.codeCharacters("\"\"") === 2)
    assert(Day8.inMemoryCharacters("\"\"") === 0)
    assert(Day8.codeCharacters("\"abc\"") === 5)
    assert(Day8.inMemoryCharacters("\"abc\"") === 3)
    assert(Day8.codeCharacters("\"aaa\\\"aaa\"") === 10)
    assert(Day8.inMemoryCharacters("\"aaa\\\"aaa\"") === 7)
    assert(Day8.codeCharacters("\"\\x27\"") === 6)
    assert(Day8.inMemoryCharacters("\"\\x27\"") === 1)
  }

  test("part 2 examples") {
    assert(Day8.escapeCharacters("\"\"") === 6)
    assert(Day8.escapeCharacters("\"abc\"") === 9)
    assert(Day8.escapeCharacters("\"aaa\\\"aaa\"") === 16)
    assert(Day8.escapeCharacters("\"\\x27\"") === 11)
  }

}

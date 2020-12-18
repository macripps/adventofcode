package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  test("examples") {
    assert(Day10.next("1") === "11")
    assert(Day10.next("11") === "21")
    assert(Day10.next("21") === "1211")
    assert(Day10.next("1211") === "111221")
    assert(Day10.next("111221") === "312211")
  }
}

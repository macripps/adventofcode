package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  test("examples given decode correctly") {
    assert(Day5.binaryToInt(Day5.toBinary("FBFBBFFRLR")) === 357)
    assert(Day5.binaryToInt(Day5.toBinary("BFFFBBFRRR")) === 567)
    assert(Day5.binaryToInt(Day5.toBinary("FFFBBBFRRR")) === 119)
    assert(Day5.binaryToInt(Day5.toBinary("BBFFBBFRLL")) === 820)
  }
}

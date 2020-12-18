package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  test("example") {
    assert(Day14.distance(14, 10, 127, 1) === 14)
    assert(Day14.distance(14, 10, 127, 10) === 140)
    assert(Day14.distance(14, 10, 127, 11) === 140)
    assert(Day14.distance(14, 10, 127, 1000) === 1120)
    assert(Day14.distance(16, 11, 162, 1) === 16)
    assert(Day14.distance(16, 11, 162, 10) === 160)
    assert(Day14.distance(16, 11, 162, 11) === 176)
    assert(Day14.distance(16, 11, 162, 1000) === 1056)
  }

}

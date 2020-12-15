package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  test("examples") {
    assert(Day15.iterate(Array(0, 3, 6), 4) === 0)
    assert(Day15.iterate(Array(0, 3, 6), 5) === 3)
    assert(Day15.iterate(Array(0, 3, 6), 6) === 3)
    assert(Day15.iterate(Array(0, 3, 6), 7) === 1)
    assert(Day15.iterate(Array(0, 3, 6), 8) === 0)
    assert(Day15.iterate(Array(0, 3, 6), 9) === 4)
    assert(Day15.iterate(Array(0, 3, 6), 10) === 0)
    assert(Day15.iterate(Array(0, 3, 6), 2020) === 436)
    assert(Day15.iterate(Array(1, 3, 2), 2020) === 1)
    assert(Day15.iterate(Array(2, 1, 3), 2020) === 10)
    assert(Day15.iterate(Array(1, 2, 3), 2020) === 27)
    assert(Day15.iterate(Array(2, 3, 1), 2020) === 78)
    assert(Day15.iterate(Array(3, 2, 1), 2020) === 438)
    assert(Day15.iterate(Array(3, 1, 2), 2020) === 1836)
  }

}

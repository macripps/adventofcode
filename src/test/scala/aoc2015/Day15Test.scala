package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  test("proportions") {
    assert(Day15.nextDown(Array(0, 0, 0, 100)) === Array(0, 0, 1, 99))
    assert(Day15.nextDown(Array(0, 0, 1, 99)) === Array(0, 0, 2, 98))
    assert(Day15.nextDown(Array(0, 0, 100, 0)) === Array(0, 1, 0, 99))
    assert(Day15.nextDown(Array(0, 100, 0, 0)) === Array(1, 0, 0, 99))
    assert(Day15.nextDown(Array(25, 25, 0, 50)) === Array(25, 25, 1, 49))
  }

  test("Score") {
    assert(Day15.score(Array(44, 56), Array(("Butterscotch", -1, -2, 6, 3, 8), ("Cinnamon", 2, 3, -2, -1, 3))) === 62842880)
  }

}

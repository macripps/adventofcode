package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  test("Chinese Remainder Theorem") {
    assert(Day13.chineseRemainerTheorem(Array((0, 3), (3, 4), (4, 5))) === (39, 60))
    assert(Day13.chineseRemainerTheorem(Array((6, 7), (4, 5), (1, 3))) === (34, 105))
    assert(Day13.chineseRemainerTheorem(
      Array(
        (0, 23),
        (28, 41),
        (486, 509),
        (3, 13),
        (14, 17),
        (6, 29),
        (347, 401),
        (14, 37),
        (3, 19),
      )) === (225850756401039L, 867200349647749L)
    )
  }

  test("Bezout Identity") {
    assert(Day13.bezoutIdentity(240, 46) === (-9, 47, 2))
    assert(Day13.bezoutIdentity(3, 4) === (-1, 1, 1))
    assert(Day13.bezoutIdentity(5, 12) === (5, -2, 1))
  }

}

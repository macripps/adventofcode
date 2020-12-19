package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  test("example") {
    val result = Day17.findSublistsThatSumsTo(List(20, 15, 10, 5, 5), 25)
    println("Result: " + result)
    assert(result.get.contains(List(15, 10)))
    assert(result.get.contains(List(20, 5)))
  }

}

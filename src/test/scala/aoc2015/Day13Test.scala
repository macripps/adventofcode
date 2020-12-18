package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  test("example") {
    val example = Map[String, Map[String, Int]](
      "Alice" -> Map("Bob" -> 54, "Carol" -> -79, "David" -> -2),
      "Bob" -> Map("Alice" -> 83, "Carol" -> -7, "David" -> -63),
      "Carol" -> Map("Alice" -> -62, "Bob" -> 60, "David" -> 55),
      "David" -> Map("Alice" -> 46, "Bob" -> -7, "Carol" -> 41)
    )

    assert(Day13.happinessCalc(example, List("David", "Alice", "Bob", "Carol")) + Day13.happinessCalc(example, List("Carol", "Bob", "Alice", "David")) === 330)
  }

}

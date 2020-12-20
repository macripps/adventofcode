package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  test("example") {
    val rules = Array(
      ("H", "HO"),
      ("H", "OH"),
      ("O", "HH"),
    )
    assert(Day19.generate(rules, "HOH").size == 4)
    assert(Day19.generate(rules, "HOHOHO").size == 7)
  }

}

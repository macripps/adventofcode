package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  test("example") {
    val rules = Array[String](
      "0: 4 1 5",
      "1: 2 3 | 3 2",
      "2: 4 4 | 5 5",
      "3: 4 5 | 5 4",
      "4: \"a\"",
      "5: \"b\"",
    )
    val reg = Day19.buildValid(rules, 0, 0).r
    println(reg)
    assert(reg.matches("aaaabb"))
    assert(reg.matches("aaabab"))
    assert(reg.matches("abbabb"))
    assert(reg.matches("abbbab"))
    assert(reg.matches("aabbbb"))
    assert(reg.matches("abaaab"))
    assert(reg.matches("aabbbb"))
    assert(reg.matches("ababbb"))
  }
}

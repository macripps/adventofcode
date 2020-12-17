package aoc2015

import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  test("examples part1") {
    assert(Day5.nicePart1("ugknbfddgicrmopn"))
    assert(Day5.nicePart1("aaa"))
    assert(!Day5.nicePart1("jchzalrnumimnmhp"))
    assert(!Day5.nicePart1("haegwjzuvuyypxyu"))
    assert(!Day5.nicePart1("dvszwmarrgswjxmb"))
  }

  test("examples part2") {
    assert(Day5.nicePart2("qjhvhtzxzqqjkmpb"))
    assert(Day5.nicePart2("xxyxx"))
    assert(!Day5.nicePart2("uurcxstgmygtbstg"))
    assert(!Day5.nicePart2("ieodomkazucvgmuy"))
  }
}

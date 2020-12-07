package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  test("shiny gold contained within bags") {
    assert(Day7.part1(Colour("shiny gold"), Set(
      Edge(Colour("light red"), Colour("bright white"), 1),
      Edge(Colour("light red"), Colour("muted yellow"), 2),
      Edge(Colour("dark orange"), Colour("bright white"), 3),
      Edge(Colour("dark orange"), Colour("muted yellow"), 4),
      Edge(Colour("bright white"), Colour("shiny gold"), 1),
      Edge(Colour("muted yellow"), Colour("shiny gold"), 2),
      Edge(Colour("muted yellow"), Colour("faded blue"), 9),
      Edge(Colour("shiny gold"), Colour("dark olive"), 1),
      Edge(Colour("shiny gold"), Colour("vibrant plum"), 2),
      Edge(Colour("dark olive"), Colour("faded blue"), 3),
      Edge(Colour("dark olive"), Colour("dotted black"), 4),
      Edge(Colour("vibrant plum"), Colour("faded blue"), 5),
      Edge(Colour("vibrant plum"), Colour("dotted black"), 6),
    )) === 4)
  }

  test("shiny gold contains bags") {
    assert(Day7.part2(Colour("shiny gold"), Set(
      Edge(Colour("shiny gold"), Colour("dark red"), 2),
      Edge(Colour("dark red"), Colour("dark orange"), 2),
      Edge(Colour("dark orange"), Colour("dark yellow"), 2),
      Edge(Colour("dark yellow"), Colour("dark green"), 2),
      Edge(Colour("dark green"), Colour("dark blue"), 2),
      Edge(Colour("dark blue"), Colour("dark violet"), 2),
    )) === 126)
  }
}

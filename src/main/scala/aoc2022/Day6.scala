package aoc2022

class Day6 extends aoc.Day(2022, 6) {
  override def part1(input: Array[String]): Any = {
    val line = input.head
    (3 until line.length).find { n =>
      line.slice(n-3, n + 1).toSet.size == 4
    }.get+1
  }

  override def part2(input: Array[String]): Any = {
    val line = input.head
    (13 until line.length).find { n =>
      line.slice(n - 13, n + 1).toSet.size == 14
    }.get+1
  }
}

object Day6 {
  def apply()= new Day6
}

package aoc2022

class Day1 extends aoc.Day(2022, 1) {
  override def part1(input: Array[String]): String = super.inputGroups(input).map(x => x.map(_.toInt).sum).max.toString

  override def part2(input: Array[String]): String = {
    super.inputGroups(input).map(x => x.map(_.toInt).sum).toSeq.sorted.takeRight(3).sum.toString
  }
}

object Day1 {
  def apply() = new Day1
}

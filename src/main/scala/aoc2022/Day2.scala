package aoc2022

class Day2 extends aoc.Day(2022, 2) {
  override def part1(input: Array[String]): String = {
    input.map { line =>
      val opponent = line.charAt(0) - 'A';
      val choice = line.charAt(2) - 'X';
      1 + choice + ((4 + choice - opponent) % 3) * 3
    }.sum.toString
  }

  override def part2(input: Array[String]): String = {
    input.map { line =>
      val opponent = line.charAt(0) - 'A';
      val result = line.charAt(2) - 'X';
      1 + ((opponent + 2 + result) % 3) + (result * 3)
    }.sum.toString
  }
}

object Day2 {
  def apply() = new Day2
}

package aoc2022

class Day3 extends aoc.Day(2022, 3) {
  val test = Array(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )
  override def part1(input: Array[String]): String = {
    input.map { line =>
      val (left, right) = line.splitAt(line.length/2)
      left.filter {
        l: Char => right.contains(l)
      }.toSet.map(toPriority).sum
    }.sum.toString

  }

  override def part2(input: Array[String]): String = {
    input.grouped(3).map { lines =>
      lines.head.filter { c =>
        lines.tail.forall(l => l.contains(c))
      }.toSet.map(toPriority).sum
    }.sum.toString
  }

  private[this] def toPriority(p: Char): Int = {
    if (p >= 'a' && p <= 'z') {
      p - '`'
    } else if (p >= 'A' && p <= 'Z') {
      p + 26 - '@'
    } else 0
  }
}

object Day3 {
  def apply() = new Day3()
}

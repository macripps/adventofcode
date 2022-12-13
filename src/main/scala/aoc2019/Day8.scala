package aoc2019

class Day8 extends aoc.Day(2019, 8) {
  override def part1(input: Array[String]): Any = {
    val minS = input.head.grouped(25 * 6).minBy(x => x.count(_ == '0'))
    minS.count(_ == '1') * minS.count(_ == '2')
  }

  override def part2(input: Array[String]): Any = {
    val layers = input.head.grouped(25 * 6).toSeq
    "\n" + (0 to 5).map { row =>
      (0 to 24).map { col =>
        val idx = row * 25 + col
        val c = layers.dropWhile(p => p.charAt(idx) == '2').head.charAt(idx)
        if (c == '1') {
          '#'
        } else {
          ' '
        }
      }.mkString
    }.mkString("\n")
  }
}

object Day8 {
  def apply() = new Day8
}

package aoc2022

class Day4 extends aoc.Day(2022, 4) {
  val test =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin.split("\n")

  override def part1(input: Array[String]): Int = input.map(toRanges).count {
    case (leftMin, leftMax, rightMin, rightMax) =>
      (leftMin <= rightMin && leftMax >= rightMax) || (rightMin <= leftMin && rightMax >= leftMax)
  }

  private[this] def toRanges(line: String): (Int, Int, Int, Int) = {
    val assignments = line.split(',')
    val left = assignments(0).split('-')
    val right = assignments(1).split('-')
    (left(0).toInt, left(1).toInt, right(0).toInt, right(1).toInt)
  }

  override def part2(input: Array[String]): Int = input.map(toRanges).count {
    case (leftMin, leftMax, rightMin, rightMax) =>
      leftMin <= rightMax && leftMax >= rightMin
  }
}

object Day4 {
  def apply() = new Day4()
}

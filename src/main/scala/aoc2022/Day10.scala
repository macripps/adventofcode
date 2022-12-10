package aoc2022

import Day10._

class Day10 extends aoc.Day(2022, 10) {
  override def part1(input: Array[String]): Any = {
    var X = 1L
    var cycleValues = Seq[Long](0)
    input.foreach {
      case addx(amt) =>
        cycleValues = cycleValues :+ X :+ X
        X = X + amt.toLong
      case _@noop =>
        cycleValues = cycleValues :+ X
    }
    (20 * cycleValues(20)) +
      (60 * cycleValues(60)) +
      (100 * cycleValues(100)) +
      (140 * cycleValues(140)) +
      (180 * cycleValues(180)) +
      (220 * cycleValues(220))
  }

  val test1 =
    """noop
      |addx 3
      |addx -5""".stripMargin.split("\n")

  val test2 =
    """addx 15
      |addx -11
      |addx 6
      |addx -3
      |addx 5
      |addx -1
      |addx -8
      |addx 13
      |addx 4
      |noop
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx 5
      |addx -1
      |addx -35
      |addx 1
      |addx 24
      |addx -19
      |addx 1
      |addx 16
      |addx -11
      |noop
      |noop
      |addx 21
      |addx -15
      |noop
      |noop
      |addx -3
      |addx 9
      |addx 1
      |addx -3
      |addx 8
      |addx 1
      |addx 5
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx -36
      |noop
      |addx 1
      |addx 7
      |noop
      |noop
      |noop
      |addx 2
      |addx 6
      |noop
      |noop
      |noop
      |noop
      |noop
      |addx 1
      |noop
      |noop
      |addx 7
      |addx 1
      |noop
      |addx -13
      |addx 13
      |addx 7
      |noop
      |addx 1
      |addx -33
      |noop
      |noop
      |noop
      |addx 2
      |noop
      |noop
      |noop
      |addx 8
      |noop
      |addx -1
      |addx 2
      |addx 1
      |noop
      |addx 17
      |addx -9
      |addx 1
      |addx 1
      |addx -3
      |addx 11
      |noop
      |noop
      |addx 1
      |noop
      |addx 1
      |noop
      |noop
      |addx -13
      |addx -19
      |addx 1
      |addx 3
      |addx 26
      |addx -30
      |addx 12
      |addx -1
      |addx 3
      |addx 1
      |noop
      |noop
      |noop
      |addx -9
      |addx 18
      |addx 1
      |addx 2
      |noop
      |noop
      |addx 9
      |noop
      |noop
      |noop
      |addx -1
      |addx 2
      |addx -37
      |addx 1
      |addx 3
      |noop
      |addx 15
      |addx -21
      |addx 22
      |addx -6
      |addx 1
      |noop
      |addx 2
      |addx 1
      |noop
      |addx -10
      |noop
      |noop
      |addx 20
      |addx 1
      |addx 2
      |addx 2
      |addx -6
      |addx -11
      |noop
      |noop
      |noop""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    var spritePosition = 1
    var cycle = 1
    var position = 0
    input.foreach { line =>
      line match {
        case addx(amt) =>
          if (math.abs(spritePosition - position) <= 1) {
            print('#')
          } else {
            print(' ')
          }
          if (cycle == 40) {
            println()
            cycle = 0
            position = -1
          }
          cycle = cycle + 1
          position = position + 1
          if (math.abs(spritePosition - position) <= 1) {
            print('#')
          } else {
            print(' ')
          }
          spritePosition = spritePosition + amt.toInt
        case _@noop =>
          if (math.abs(spritePosition - position) <= 1) {
            print('#')
          } else {
            print(' ')
          }
      }
      if (cycle == 40) {
        println()
        cycle = 0
        position = -1
      }
      cycle = cycle + 1
      position = position + 1
    }
    ""
  }
}

object Day10 {
  def apply() = new Day10

  val noop = raw"noop".r
  val addx = raw"addx (-?\d+)".r
}

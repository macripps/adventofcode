package aoc2023

import scala.annotation.tailrec

class Day1 extends aoc.Day(2023, 1) {

  import Day1._

  withPart1Test(
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin, 142)

  override def part1(input: Array[String]): Any = input.map { line =>
    val left = line.dropWhile(c => !c.isDigit).head
    val right = line.reverse.dropWhile(c => !c.isDigit).head
    (left - '0') * 10 + (right - '0')
  }.sum

  withPart2Test("""two1nine
                  |eightwothree
                  |abcone2threexyz
                  |xtwone3four
                  |4nineeightseven2
                  |zoneight234
                  |7pqrstsixteen""".stripMargin, 281)

  override def part2(input: Array[String]): Any = input.map { line =>
    val left = firstNumberOrName(line)
    val right = lastNumberOrName(line)
    left * 10 + right
  }.sum
}

object Day1 {
  def apply() = new Day1

  @tailrec
  def firstNumberOrName(line: String): Int = {
    if (line.head.isDigit) {
      line.head - '0'
    } else if (line.startsWith("one")) {
      1
    } else if (line.startsWith("two")) {
      2
    } else if (line.startsWith("three")) {
      3
    } else if (line.startsWith("four")) {
      4
    } else if (line.startsWith("five")) {
      5
    } else if (line.startsWith("six")) {
      6
    } else if (line.startsWith("seven")) {
      7
    } else if (line.startsWith("eight")) {
      8
    } else if (line.startsWith("nine")) {
      9
    } else if (line.startsWith("zero")) {
      0
    } else firstNumberOrName(line.tail)
  }

  @tailrec
  def lastNumberOrName(line: String): Int = {
    if (line.last.isDigit) {
      line.last - '0'
    } else if (line.endsWith("one")) {
      1
    } else if (line.endsWith("two")) {
      2
    } else if (line.endsWith("three")) {
      3
    } else if (line.endsWith("four")) {
      4
    } else if (line.endsWith("five")) {
      5
    } else if (line.endsWith("six")) {
      6
    } else if (line.endsWith("seven")) {
      7
    } else if (line.endsWith("eight")) {
      8
    } else if (line.endsWith("nine")) {
      9
    } else if (line.endsWith("zero")) {
      0
    } else lastNumberOrName(line.dropRight(1))
  }
}

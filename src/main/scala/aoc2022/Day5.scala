package aoc2022

import scala.collection.mutable

class Day5 extends aoc.Day(2022, 5) {
  override def part1(input: Array[String]): Any = {
    val puzzle = inputGroups(input)
    val stacks = buildStacks(puzzle.head)
    val algorithm = puzzle.tail.head

    algorithm.foreach {
      case algoLine(amount, src, dest) =>
        val srcStack = stacks(src.toInt - 1)
        val destStack = stacks(dest.toInt - 1)
        (1 to amount.toInt).foreach { _ =>
          val x = srcStack.pop()
          destStack.push(x)
        }
    }
    stacks.map { s =>
      s.head
    }.mkString
  }

  val algoLine = raw"move (\d+) from (\d) to (\d)".r
  private[this] def buildStacks(stackLines: Iterable[String]) = {
    val stacks = Array(
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
      mutable.Stack[Char](),
    )

    stackLines.dropRight(1).foreach { stackLine =>
      val line = stackLine.grouped(4)
      line.map(_.trim).zipWithIndex.filter { case (d: String, _: Int) => d.nonEmpty }.foreach {
        case (d: String, i: Int) => stacks(i).append(d.charAt(1))
      }
    }
    stacks
  }

  val test = """    [D]
               |[N] [C]
               |[Z] [M] [P]
               | 1   2   3
               |
               |move 1 from 2 to 1
               |move 3 from 1 to 3
               |move 2 from 2 to 1
               |move 1 from 1 to 2""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val puzzle = inputGroups(input)
    val stacks = buildStacks(puzzle.head)
    val algorithm = puzzle.tail.head

    algorithm.foreach {
      case algoLine(amount, src, dest) =>
        val srcStack = stacks(src.toInt - 1)
        val destStack = stacks(dest.toInt - 1)
        val amt = amount.toInt
        val x = srcStack.take(amt)
        stacks(src.toInt - 1) = srcStack.drop(amt)
        destStack.prependAll(x)
    }
    stacks.map { s =>
      s.head
    }.mkString
  }
}

object Day5 {
  def apply() = new Day5()
}

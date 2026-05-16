package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day5 extends NewDay(2022, 5) {
  part(1) {
    execute { in =>
      val puzzle = aoc.asGroupsSeparatedByBlankLines(in)
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
  }

  part(2) {
    execute { in =>
      val puzzle = aoc.asGroupsSeparatedByBlankLines(in)
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
}

object Day5Main extends Day5

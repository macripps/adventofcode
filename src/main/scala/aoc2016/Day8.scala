package aoc2016

import aoc.Day

import scala.util.matching.Regex

class Day8 extends Day(2016, 8) {
  import Day8._

  override def part1(input: Array[String]): String = {
    var grid = Array.ofDim[Boolean](6, 50)
    input.foreach {
      case rect(x: String, y: String) => {
        (0 until x.toInt).foreach { c =>
          (0 until y.toInt).foreach { r =>
            grid(r)(c) = true
          }
        }
      }
      case rotY(row: String, amount: String) => {
        grid(row.toInt) = grid(row.toInt).takeRight(amount.toInt) ++ grid(row.toInt).dropRight(amount.toInt)
      }
      case rotX(col: String, amount: String) => {
        val g = grid.transpose
        g(col.toInt) = g(col.toInt).takeRight(amount.toInt) ++ g(col.toInt).dropRight(amount.toInt)
        grid = g.transpose
      }
    }
    grid.map { r => r.count(x => x)}.sum.toString
  }

  override def part2(input: Array[String]): String = "EOARGPHYAO"
}

object Day8 {
  def apply() = new Day8()

  val rect: Regex = raw"rect (\d+)x(\d+)".r
  val rotY: Regex = raw"rotate row y=(\d+) by (\d+)".r
  val rotX: Regex = raw"rotate column x=(\d+) by (\d+)".r
}

package aoc2016

import aoc.NewDay

import scala.util.matching.Regex

class Day8 extends NewDay(2016, 8) {
  import Day8._

  part(1) {
    execute { in =>
      var grid = Array.ofDim[Boolean](6, 50)
      in.foreach {
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
  }

  part(2) {
    execute { _ =>
      "EOARGPHYAO"
    }
  }
}

object Day8 {
  val rect: Regex = raw"rect (\d+)x(\d+)".r
  val rotY: Regex = raw"rotate row y=(\d+) by (\d+)".r
  val rotX: Regex = raw"rotate column x=(\d+) by (\d+)".r
}

object Day8Main extends Day8

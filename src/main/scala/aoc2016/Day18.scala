package aoc2016

import aoc.NewDay

class Day18 extends NewDay(2016, 18) {
  part(1) {
    execute { in =>
      var row = in(0)
      var spaces = row.count(_ == '.')
      (2 to 40).foreach { _ =>
        row = iterate(row)
        spaces = spaces + row.count(_ == '.')
      }
      spaces.toString
    }
  }

  def iterate(str: String): String = {
    val x = new StringBuilder
    val z = "." + str + "."
    z.indices.drop(1).dropRight(1).foreach { i =>
      toI(z(i-1)) * 4 + toI(z(i)) * 2 + toI(z(i+1)) match {
        case 6 | 4 | 3 | 1 => x.append('^')
        case _ => x.append('.')
      }
    }
    x.result()
  }

  def toI(c: Char): Int = {
    c match {
      case '^' => 1
      case '.' => 0
    }
  }

  part(2) {
    execute { in =>
      var row = in(0)
      var spaces = row.count(_ == '.')
      (2 to 400000).foreach { _ =>
        row = iterate(row)
        spaces = spaces + row.count(_ == '.')
      }
      spaces.toString
    }
  }
}

object Day18Main extends Day18

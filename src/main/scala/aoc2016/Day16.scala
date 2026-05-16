package aoc2016

import aoc.NewDay

class Day16 extends NewDay(2016, 16) {
  part(1) {
    execute { in =>
      val length = 272
      val x = in(0)
      checksum(f(x, length).take(length))
    }
  }

  def f(x: String, length: Int): String = {
    val z = new StringBuilder(length)
    z.append(x)
    while (z.length() <= length) {
      val y = z.toString().reverse
      z.append('0')
      y.foreach {
        case '1' => z.append('0')
        case '0' => z.append('1')
      }
    }
    z.result()
  }

  def checksum(str: String): String = {
    var x = new StringBuilder(str)
    while (x.length % 2 == 0) {
      Range.inclusive(0, x.length-2, 2).foreach { i =>
        if (x(i) == x(i+1)) {
          x(i/2) = '1'
        } else {
          x(i/2) = '0'
        }
      }
      x.length_=(x.length/2)
    }
    x.result()
  }

  part(2) {
    execute { in =>
      val length = 35651584
      val x = in(0)
      checksum(f(x, length).take(length))
    }
  }
}

object Day16Main extends Day16

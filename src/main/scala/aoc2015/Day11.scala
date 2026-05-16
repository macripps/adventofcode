package aoc2015

import aoc.NewDay
import Day11._

class Day11 extends NewDay(2015, 11) {
  part(1) {
    execute { in =>
      next(in(0))
    }
  }

  part(2) {
    execute { in =>
      next(next(in(0)))
    }
  }
}

object Day11 {
  def next(current: String): String = {
    var password = increment(current)
    while (!valid(password)) {
      password = increment(password)
    }
    password
  }

  def increment(password: String): String = {
    if (password.last < 'z') {
      password.dropRight(1) + (password.last+1).toChar
    } else {
      increment(password.dropRight(1)) + 'a'
    }
  }

  def valid(password: String): Boolean = {
    {
      (0 to password.length - 3).exists { i =>
        password(i+1) == (password(i) + 1) &&
          password(i+2) == (password(i+1) + 1)
      }
    } &&
    !(password.contains('i') || password.contains('o') || password.contains('l')) && {
      val firstPair = (0 to password.length - 3).find { i =>
        password(i+1) == password(i)
      }
      firstPair match {
        case None => false
        case Some(i) =>
          (i+2 until password.length-1).exists { i =>
            password(i+1) == password(i)
          }
      }
    }
  }
}

object Day11Main extends Day11

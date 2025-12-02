package aoc2025

import aoc.NewDay

class Day1 extends NewDay(2025, 1) {
  part(1) {
    test("""L68
           |L30
           |R48
           |L5
           |R60
           |L55
           |L1
           |L99
           |R14
           |L82""".stripMargin -> 3)
    execute { in =>
      var at = 50
      var matches = 0
      in.foreach { s =>
        at = (s.head, s.tail) match {
          case ('L', d) => at- d.toInt
          case ('R', d) => at + d.toInt
        }
        if (at < 0) at = at + 100
        at = at % 100
        if (at == 0) {
          matches = matches+1
        }
      }
      matches
    }
  }

  part(2) {
    test("""L68
           |L30
           |R48
           |L5
           |R60
           |L55
           |L1
           |L99
           |R14
           |L82""".stripMargin -> 6)
    execute { in =>
      var at = 50
      var matches = 0
      in.foreach { s =>
        (s.head, s.tail.toInt) match {
          case ('L', d) =>
            (1 to d).foreach { _ =>
              at = at - 1
              if (at < 0) at = at + 100
              if (at == 0) {
                matches = matches + 1
              }
            }
          case ('R', d) =>
            (1 to d).foreach { _ =>
              at = at + 1
              if (at > 99) at = at - 100
              if (at == 0) {
                matches = matches + 1
              }
            }
        }
        println(s, at, matches)
      }
      matches
    }

  }

}

object Day1Main extends Day1 {

}
package aoc2024

import aoc.{NewDay, asGroupsSeparatedByBlankLines}
import com.microsoft.z3.{Context, FuncDecl, Status}

class Day13 extends NewDay(2024, 13) {
  part(1) {
    test(
      """Button A: X+94, Y+34
        |Button B: X+22, Y+67
        |Prize: X=8400, Y=5400
        |
        |Button A: X+26, Y+66
        |Button B: X+67, Y+21
        |Prize: X=12748, Y=12176
        |
        |Button A: X+17, Y+86
        |Button B: X+84, Y+37
        |Prize: X=7870, Y=6450
        |
        |Button A: X+69, Y+23
        |Button B: X+27, Y+71
        |Prize: X=18641, Y=10279""".stripMargin -> 480)
    execute { ls =>
      val insts = asGroupsSeparatedByBlankLines(ls)
      insts.map { ls =>
        val lA = ls.head
        val lB = ls.tail.head
        val p = ls.tail.tail.head
        val Array(daX, daY) = lA.split(": ")(1).split(", ").map(_.drop(2).toLong)
        val Array(dbX, dbY) = lB.split(": ")(1).split(", ").map(_.drop(2).toLong)
        val Array(pX, pY) = p.split(": ")(1).split(", ").map(_.drop(2).toLong)

        var min = Long.MaxValue
        (0L to 100L).foreach { nA =>
          val nbX = (pX - (daX * nA)) / dbX
          val nbY = (pY - (daY * nA)) / dbY
          if (nbX == nbY && (nbX * dbX + daX * nA) == pX && (nbY * dbY + daY * nA) == pY) {
            val cost = 3 * nA + nbX
            if (cost < min) {
              if (debug()) println(nA, nbX)
              min = cost
            }
          }
        }
        if (debug()) println(daX, daY, dbX, dbY, pX, pY, min)
        if (debug()) println(min)
        if (min == Long.MaxValue) 0L else min
      }.sum
    }
  }
  part(2) {
    execute { ls =>
      println("See python implementation")
      0
    }
  }
}

object Day13Main extends Day13

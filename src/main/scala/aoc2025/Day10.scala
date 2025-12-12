package aoc2025

import aoc.NewDay
import com.microsoft.z3.{Context, Status}

class Day10 extends NewDay(2025, 10) {

  part(1) {
    test("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" -> 2)
    test("[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" -> 3)
    test("[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" -> 2)
    execute { ls =>
      ls.map { l =>
        val split = l.split(' ')
        val desiredLights = split.head.drop(1).dropRight(1).map(c => c == '#')
        val wiring = split.drop(1).dropRight(1).map { w =>
          w.drop(1).dropRight(1).split(',').map(_.toInt).toSet
        }.toSet
        wiring.subsets().toSet.filter { ws =>
          val lighting = Array.ofDim[Boolean](desiredLights.length)
          ws.foreach { buttons =>
            buttons.foreach { idx =>
              lighting(idx) = !lighting(idx)
            }
          }
          lighting.sameElements(desiredLights)
        }.minBy(_.size).size
      }.sum
    }
  }

  part(2) {
    test("[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" -> 10)
    test("[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" -> 12)
    test("[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" -> 11)
    execute { ls =>
      ls.map { l =>
        val data = l.split(' ')
        val buttons = data.drop(1).dropRight(1).map { y =>
          y.drop(1).dropRight(1).split(',').map(_.toInt)
        }
        val joltage = data.last.drop(1).dropRight(1).split(',').map(_.toInt)

        val buttonAdds = buttons.map { bs =>
          val perButtonAdds = Array.fill[Int](joltage.length)(0)
          bs.foreach { k =>
            perButtonAdds(k) = 1
          }
          perButtonAdds
        }

        val c = new Context()
        val optimize = c.mkOptimize()

        val presses = buttons.indices.map { i =>
          val p = c.mkIntConst("p" + i)
          optimize.Add(c.mkGe(p, c.mkInt(0)))
          p
        }

        val outs = joltage.indices.map { v =>
          c.mkAdd(presses.zip(buttonAdds).map { case (p,b) =>
            c.mkMul(p, c.mkInt(b(v)))
          }:_*).simplify()
        }

        outs.indices.foreach { j =>
          optimize.Add(c.mkEq(outs(j), c.mkInt(joltage(j))))
        }

        val min = optimize.MkMinimize(c.mkAdd(presses:_*))
        val result = optimize.Check() match {
          case Status.SATISFIABLE => min.getValue
          case Status.UNKNOWN => throw new Exception(optimize.getReasonUnknown)
          case Status.UNSATISFIABLE => throw new Exception("Unsatisfiable")
        }

        result.toString.toLong
      }.sum

    }
  }
}

object Day10Main extends Day10

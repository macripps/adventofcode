package aoc2021

import aoc.NewDay

class Day6 extends NewDay(2021, 6) {
  part(1) {
    execute { in =>
      var remaining = in.head.split(',').map(_.toInt).toList
      (1 to 80).foreach { _ =>
        val newFish = remaining.filter(_ == 0).map(_ => 8)
        val existingFish = remaining.map(n => if (n == 0) 6 else n -1)
        remaining = existingFish ++ newFish
      }
      remaining.size.toString
    }
  }

  part(2) {
    execute { in =>
      val fish = in.head.split(',').map(_.toInt).toList
      var fishMap: Map[Int,Long] = (0 to 8).map(n => n -> fish.count(a => a == n).toLong).toMap
      (1 to 256).foreach { _ =>
        fishMap = Map(
          0 -> fishMap(1),
          1 -> fishMap(2),
          2 -> fishMap(3),
          3 -> fishMap(4),
          4 -> fishMap(5),
          5 -> fishMap(6),
          6 -> fishMap(7).+(fishMap(0)),
          7 -> fishMap(8),
          8 -> fishMap(0),
        )
      }
      fishMap.values.sum.toString
    }
  }
}

object Day6Main extends Day6

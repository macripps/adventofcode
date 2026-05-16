package aoc2022

import aoc.NewDay

import scala.collection.mutable
import Day15._

import scala.util.control.Breaks.{break, breakable}

class Day15 extends NewDay(2022, 15) {
  part(1) {
    execute { in =>
      val sensors = mutable.Set[aoc.Point]()
      val beacons = mutable.Set[aoc.Point]()
      var minX = Integer.MAX_VALUE
      var maxX = Integer.MIN_VALUE
      val sensorBeaconPairs = mutable.Set[(aoc.Point, aoc.Point)]()
      in.foreach {
        case pLine(sX, sY, bX, bY) => {
          val sensor = aoc.Point(sX.toInt, sY.toInt)
          val beacon = aoc.Point(bX.toInt, bY.toInt)
          sensors += sensor
          beacons += beacon
          sensorBeaconPairs += ((sensor, beacon))
          val d = sensor.manhattanDistanceTo(beacon)
          minX = math.min(sensor.x - d, minX)
          maxX = math.max(sensor.x + d, maxX)
        }
      }
      val y = 2_000_000
      var impossible = 0L
      (minX to maxX).foreach { x =>
        val pt = aoc.Point(x, y)
        if (!beacons.contains(pt)) {
          if (sensorBeaconPairs.exists(p =>
            pt.manhattanDistanceTo(p._1) <= p._1.manhattanDistanceTo(p._2)
          )) {
            impossible = impossible + 1
          }
        }
      }
      impossible
    }
  }

  part(2) {
    execute { in =>
      // Based on https://github.com/MathCroc/aoc/blob/master/2022/day15/main.cpp
      val coverage = mutable.Buffer[(aoc.Point, Int)]()
      in.foreach {
        case pLine(sX, sY, bX, bY) => {
          val sensor = aoc.Point(sX.toInt, sY.toInt)
          val beacon = aoc.Point(bX.toInt, bY.toInt)
          val d = sensor.manhattanDistanceTo(beacon)
          coverage += ((sensor, d))
        }
      }
      breakable {
        coverage.indices.foreach { i =>
          (i + 1 until coverage.size).foreach { j =>
            val (a, da) = coverage(i)
            val (b, db) = coverage(j)
            val distance = a.manhattanDistanceTo(b)
            if (distance - 1 <= da + db) {
              var p = intersection(1, -(a.x - da) + a.y, -1, (b.x - db) + b.y)
              if (!checkCoverage(coverage, p.x - 1, p.y)) {
                break
              }
              p = intersection(1, -(a.x - da) + a.y, -1, (b.x + db) + b.y)
              if (!checkCoverage(coverage, p.x, p.y + 1)) {
                break
              }
              p = intersection(1, -(a.x + da) + a.y, -1, (b.x - db) + b.y)
              if (!checkCoverage(coverage, p.x, p.y - 1)) {
                break
              }
              p = intersection(1, -(a.x + da) + a.y, -1, (b.x + db) + b.y)
              if (!checkCoverage(coverage, p.x + 1, p.y)) {
                break
              }
              p = intersection(-1, (a.x + da) + a.y, 1, -(b.x + db) + b.y)
              if (!checkCoverage(coverage, p.x + 1, p.y)) {
                break
              }
              p = intersection(-1, (a.x + da) + a.y, 1, -(b.x - db) + b.y);
              if (!checkCoverage(coverage, p.x, p.y + 1)) {
                break
              }
              p = intersection(-1, (a.x - da) + a.y, 1, -(b.x + db) + b.y);
              if (!checkCoverage(coverage, p.x, p.y - 1)) {
                break
              }
              p = intersection(-1, (a.x - da) + a.y, 1, -(b.x - db) + b.y);
              if (!checkCoverage(coverage, p.x - 1, p.y)) {
                break
              }
            }
          }
        }
      }
      ""
    }
  }

  private[this] def intersection(ka: Int, a: Int, kb: Int, b: Int): aoc.Point = {
    val x = (b - a) / (ka - kb)
    aoc.Point(x, ka * x + a)
  }

  private[this] def checkCoverage(coverage: mutable.Buffer[(aoc.Point, Int)], x: Int, y: Int): Boolean = {
    if (x < 0 || x > 4_000_000 || y < 0 || y > 4_000_000) {
      true
    } else {
      val pt = aoc.Point(x, y)
      val exists = coverage.exists { case (pos, d) =>
        val distance = pt.manhattanDistanceTo(pos)
        distance <= d
      }
      if (!exists) {
        println(x * 4_000_000L + y)
      }
      exists
    }
  }
}

object Day15Main extends Day15

object Day15 {
  val pLine = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r
}

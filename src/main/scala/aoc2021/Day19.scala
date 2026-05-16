package aoc2021

import aoc.{NewDay, asGroupsSeparatedByBlankLines}
import aoc2021.Day19.{Grid3, Point3, rotations}

import scala.collection.mutable

class Day19 extends NewDay(2021, 19) {
  part(1) {
    execute { in =>
      var idx = 0
      val arrays = asGroupsSeparatedByBlankLines(in).map { g =>
        val n = Grid3(idx, g.tail.map { s =>
          val p = s.split(',')
          Point3(p(0).toInt, p(1).toInt, p(2).toInt)
        }.toSet)
        idx = idx + 1
        n
      }.toArray

      var known = Seq(arrays.head)
      val unknown = mutable.Queue[Grid3]()
      arrays.tail.foreach { g =>
        unknown.addOne(g)
      }

      arrays.foreach { a1 =>
        arrays.foreach { a2 =>
          if (a1.id != a2.id) {
            a2.allRotations.foreach { r =>
              val o = a1.overlappingPoints(r)
              if (o.points.size >= 12) {
                println("Found best overlap between " + a1.id + " and " + a2.id + " at " + o.p + " with size " + o.points.size)
              }
            }
          }
        }
      }

      while (unknown.nonEmpty) {
        println(known.map(_.id))
        val g = unknown.dequeue()
        val allRotations = g.allRotations
        val overlaps = allRotations.flatMap { r =>
          known.map { k =>
            k.overlappingPoints(r)
          }.filter(_.points.size >= 12)
        }
        if (overlaps.nonEmpty) {
          val overlap = overlaps.head
          val rot = overlap.rotation
          println("Offsetting " + rot.id + " by " + (overlap.p))
          known = known :+ rot
        } else {
          unknown.addOne(g)
        }
      }
      val x = known.flatMap { g =>
        g.points
      }.toSet

          println(x.toArray.sortBy(_.x).mkString(","))
      x.size.toString
    }
  }

  part(2) {
    execute { _ =>
      ""
    }
  }
}

object Day19 {
  case class Point3(x: Int, y: Int, z: Int) {
    override def toString: String = "(" + x + "," + y + "," + z + ")"
  }

  val rotations: Seq[Point3 => Point3] = {
    Seq(
      p => Point3( p.x, p.y, p.z), p => Point3( p.x, p.y, -p.z), p => Point3( p.x, -p.y, p.z), p => Point3( p.x, -p.y, -p.z),
      p => Point3(-p.x, p.y, p.z), p => Point3(-p.x, p.y, -p.z), p => Point3(-p.x, -p.y, p.z), p => Point3(-p.x, -p.y, -p.z),
      p => Point3( p.x, p.z, p.y), p => Point3( p.x, p.z, -p.y), p => Point3( p.x, -p.z, p.y), p => Point3( p.x, -p.z, -p.y),
      p => Point3(-p.x, p.z, p.y), p => Point3(-p.x, p.z, -p.y), p => Point3(-p.x, -p.z, p.y), p => Point3(-p.x, -p.z, -p.y),
      p => Point3( p.y, p.x, p.z), p => Point3( p.y, p.x, -p.z), p => Point3( p.y, -p.x, p.z), p => Point3( p.y, -p.x, -p.z),
      p => Point3(-p.y, p.x, p.z), p => Point3(-p.y, p.x, -p.z), p => Point3(-p.y, -p.x, p.z), p => Point3(-p.y, -p.x, -p.z),
      p => Point3( p.y, p.z, p.x), p => Point3( p.y, p.z, -p.x), p => Point3( p.y, -p.z, p.x), p => Point3( p.y, -p.z, -p.x),
      p => Point3(-p.y, p.z, p.x), p => Point3(-p.y, p.z, -p.x), p => Point3(-p.y, -p.z, p.x), p => Point3(-p.y, -p.z, -p.x),
      p => Point3( p.z, p.x, p.y), p => Point3( p.z, p.x, -p.y), p => Point3( p.z, -p.x, p.y), p => Point3( p.z, -p.x, -p.y),
      p => Point3(-p.z, p.x, p.y), p => Point3(-p.z, p.x, -p.y), p => Point3(-p.z, -p.x, p.y), p => Point3(-p.z, -p.x, -p.y),
      p => Point3( p.z, p.y, p.x), p => Point3( p.z, p.y, -p.x), p => Point3( p.z, -p.y, p.x), p => Point3( p.z, -p.y, -p.x),
      p => Point3(-p.z, p.y, p.x), p => Point3(-p.z, p.y, -p.x), p => Point3(-p.z, -p.y, p.x), p => Point3(-p.z, -p.y, -p.x),
    )
  }

  case class Grid3(id: Int, points: Set[Point3]) {
    def allRotations: Seq[Grid3] = rotations.map { f => Grid3(id, points.map(f)) }

    def overlappingPoints(otherGrid: Grid3): Overlap = {
      val fixPoint = points.head
      val best = otherGrid.points.map { possibleAnchor =>
        val offset = Point3(fixPoint.x - possibleAnchor.x, fixPoint.y - possibleAnchor.y, fixPoint.z - possibleAnchor.z)
        (possibleAnchor, points.intersect(otherGrid.offsetBy(offset).points))
      }.maxBy(_._2.size)
      val bestAnchor = best._1
      val offset = Point3(fixPoint.x - bestAnchor.x, fixPoint.y - bestAnchor.y, fixPoint.z - bestAnchor.z)
      Overlap(offset, otherGrid.offsetBy(offset), best._2)
    }

    def offsetBy(p: Point3): Grid3 = {
      Grid3(id, points.map { p2 => Point3(p.x + p2.x, p.y + p2.y, p.z + p2.z) })
    }
  }

  case class Overlap(p: Point3, rotation: Grid3, points: Set[Point3])
}

object Day19Main extends Day19

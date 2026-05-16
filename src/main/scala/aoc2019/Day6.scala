package aoc2019

import aoc.NewDay

import scala.collection.mutable

class Day6 extends NewDay(2019, 6) {
  part(1) {
    test(
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L""".stripMargin -> 42L
    )
    execute { in =>
      val allobjects = mutable.Map[String, CelestialObject]().withDefault(n => new CelestialObject(n))
      in.foreach { line =>
        val idx = line.indexOf(")")
        val leftId = line.take(idx)
        val rightId = line.drop(idx + 1)
        val left = allobjects(leftId)
        allobjects.addOne((leftId, left))
        val right = allobjects(rightId)
        right.orbiting = Some(allobjects(leftId))
        allobjects.addOne((rightId, right))
      }
      val orbitCache = mutable.Map[CelestialObject, Long]()
      def children(co: CelestialObject): Long =
        orbitCache.getOrElseUpdate(co,
          if (co.orbiting.isEmpty) 0L else 1L + children(co.orbiting.get)
        )
      allobjects.values.map(children).sum
    }
  }

  part(2) {
    test(
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN""".stripMargin -> 4
    )
    execute { in =>
      val allobjects = mutable.Map[String, CelestialObject]().withDefault(n => new CelestialObject(n))
      in.foreach { line =>
        val idx = line.indexOf(")")
        val leftId = line.take(idx)
        val rightId = line.drop(idx + 1)
        val left = allobjects(leftId)
        allobjects.addOne((leftId, left))
        val right = allobjects(rightId)
        right.orbiting = Some(allobjects(leftId))
        allobjects.addOne((rightId, right))
      }
      var you = allobjects("YOU").orbiting.get
      var san = allobjects("SAN").orbiting.get
      val youPath = mutable.Buffer[CelestialObject]()
      while (you.orbiting.isDefined) {
        youPath.prepend(you)
        you = you.orbiting.get
      }
      val sanPath = mutable.Buffer[CelestialObject]()
      while (san.orbiting.isDefined) {
        sanPath.prepend(san)
        san = san.orbiting.get
      }
      val common = youPath.intersect(sanPath)
      youPath.length - common.length + sanPath.length - common.length
    }
  }
}

object Day6Main extends Day6

class CelestialObject(val id: String) {
  var orbiting: Option[CelestialObject] = None
}

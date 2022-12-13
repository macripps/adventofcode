package aoc2019

import scala.collection.mutable

class Day6 extends aoc.Day(2019, 6) {
  override def part1(input: Array[String]): Any = {
    val allobjects = mutable.Map[String, CelestialObject]().withDefault(n => new CelestialObject(n))
    input.foreach { line =>
      val idx = line.indexOf(")")
      val leftId = line.take(idx)
      val rightId = line.drop(idx + 1)
      val left = allobjects(leftId)
      allobjects.addOne((leftId, left))
      val right = allobjects(rightId)
      right.orbiting = Some(allobjects(leftId))
      allobjects.addOne((rightId, right))
    }
    var x = 0L
    allobjects.values.foreach { i =>
      x = x + children(i)
    }
    x
  }

  val orbitCache = mutable.Map[CelestialObject, Long]()

  def children(co: CelestialObject): Long = {
    if (orbitCache.contains(co)) { orbitCache(co) } else {
      println("Cache miss for " + co.id)
      if (co.orbiting.isEmpty) {
        0L
      } else {
        val n = 1 + children(co.orbiting.get)
        orbitCache(co) = n
        n
      }
    }
  }

  val test =
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
      |K)L""".stripMargin.split("\n")

  override def part2(input: Array[String]): Any = {
    val allobjects = mutable.Map[String, CelestialObject]().withDefault(n => new CelestialObject(n))
    input.foreach { line =>
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

  val test2 = """COM)B
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
                |I)SAN""".stripMargin.split("\n")
}

object Day6 {
  def apply() = new Day6

}

class CelestialObject(val id: String) {
  var orbiting: Option[CelestialObject] = None
}

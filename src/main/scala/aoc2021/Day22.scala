package aoc2021

import aoc.Day
import aoc2021.Day22._

class Day22 extends Day(2021, 22) {
  override def part1(input: Array[String]): String = {
    val steps = example1.map(parse).filter(p => p.x0 >= -50 &&
      p.x1 <= 50 &&
      p.y0 >= -50 &&
      p.y1 <= 50 &&
      p.z0 >= -50 &&
      p.z1 <= 50
    )
    var regions = Set[Region](steps.head)
    steps.tail.foreach { step =>
      println(regions, step, regions.map(_.volume).sum.toString)
      var newRegions = Set[Region]()
      regions.foreach { r =>
        newRegions = newRegions ++ merge(r, step)
      }
      regions = newRegions.filter(_.state == 1)
    }
    println(regions, regions.map(_.volume).sum.toString)
    regions.map(_.volume).sum.toString
  }

  def merge(o: Region, n: Region): Set[Region] = {
    if (o.x0 > n.x1 || o.x1 < n.x0 ||
      o.y0 > n.y1 || o.y1 < n.y0 ||
      o.z0 > n.z1 || o.z1 < n.z0) {
      // Overlap
      Set(o, n)
    } else {
      if (o.x0 < n.x0) {
        if (o.x1 > n.x1) {
          val oa = Region(o.state, o.x0,     n.x0 - 1, o.y0, o.y1, o.z0, o.z1)
          val ob = Region(o.state, n.x0,     n.x1,     o.y0, o.y1, o.z0, o.z1)
          val oc = Region(o.state, n.x1 + 1, o.x1,     o.y0, o.y1, o.z0, o.z1)
          Set(oa, oc) ++ merge(ob, n)
        } else {
          val oa = Region(o.state, o.x0, n.x0 - 1, o.y0, o.y1, o.z0, o.z1)
          val ob = Region(o.state, n.x0, o.x1,     o.y0, o.y1, o.z0, o.z1)
          Set(oa) ++ merge(ob, n)
        }
      } else if (o.x1 > n.x1) {
        val oa = Region(o.state, o.x0,     n.x1, o.y0, o.y1, o.z0, o.z1)
        val ob = Region(o.state, n.x1 + 1, o.x1, o.y0, o.y1, o.z0, o.z1)
        Set(ob) ++ merge(oa, n)
      } else if (o.y0 < n.y0) {
        if (o.y1 > n.y1) {
          val oa = Region(o.state, o.x0, o.x1, o.y0,     n.y0 - 1, o.z0, o.z1)
          val ob = Region(o.state, o.x0, o.x1, n.y0,     n.y1,     o.z0, o.z1)
          val oc = Region(o.state, o.x0, o.x1, n.y1 + 1, o.y1,     o.z0, o.z1)
          Set(oa, oc) ++ merge(ob, n)
        } else {
          val oa = Region(o.state, o.x0, o.x1, o.y0, n.y0 - 1, o.z0, o.z1)
          val ob = Region(o.state, o.x0, o.x1, n.y0, o.y1,     o.z0, o.z1)
          Set(oa) ++ merge(ob, n)
        }
      } else if (o.y1 > n.y1) {
        val oa = Region(o.state, o.x0, o.x1, o.y0,     n.y1, o.z0, o.z1)
        val ob = Region(o.state, o.x0, o.x1, n.y1 + 1, o.y1, o.z0, o.z1)
        Set(ob) ++ merge(oa, n)
      } else if (o.z0 < n.z0) {
        if (o.z1 > n.z1) {
          val oa = Region(o.state, o.x0, o.x1, o.y0, o.y1, o.z0,     n.z0 - 1)
          val ob = Region(o.state, o.x0, o.x1, o.y0, o.y1, n.z0,     n.z1)
          val oc = Region(o.state, o.x0, o.x1, o.y0, o.y1, n.z1 + 1, o.z1)
          Set(oa, oc) ++ merge(ob, n)
        } else {
          val oa = Region(o.state, o.x0, o.x1, o.y0, o.y1, o.z0, n.z0 - 1)
          val ob = Region(o.state, o.x0, o.x1, o.y0, o.y1, n.z0, o.z1)
          Set(oa) ++ merge(ob, n)
        }
      } else if (o.z1 > n.z1) {
        val oa = Region(o.state, o.x0, o.x1, o.y0, o.y1, o.z0,     n.z1)
        val ob = Region(o.state, o.x0, o.x1, o.y0, o.y1, n.z1 + 1, o.z1)
        Set(ob) ++ merge(oa, n)
      } else {
        Set(n)
      }
    }
  }

  override def part2(input: Array[String]): String = {
    ""
  }
}

object Day22 {
  def apply() = new Day22

  def parse(line: String): Region = {
    val ls = line.split(' ')
    val command = ls(0)
    val rs = ls(1).split(',')
    val xs = rs(0).drop(2).split("\\.\\.")
    val minX = xs(0).toInt
    val maxX = xs(1).toInt
    val ys = rs(1).drop(2).split("\\.\\.")
    val minY = ys(0).toInt
    val maxY = ys(1).toInt
    val zs = rs(2).drop(2).split("\\.\\.")
    val minZ = zs(0).toInt
    val maxZ = zs(1).toInt
    if (command == "on") {
      Region(1, minX, maxX, minY, maxY, minZ, maxZ)
    } else {
      Region(0, minX, maxX, minY, maxY, minZ, maxZ)
    }
  }

  case class Region(state: Int, x0: Int, x1: Int, y0: Int, y1: Int, z0: Int, z1: Int) {
    def volume: Int = (x1 + 1 - x0) * (y1 + 1 - y0) * (z1 + 1 - z0)
  }

  val example1 =
    """on x=10..12,y=10..12,z=10..12
      |on x=11..13,y=11..13,z=11..13
      |off x=9..11,y=9..11,z=9..11
      |on x=10..10,y=10..10,z=10..10""".stripMargin.split("\n")

  val example2 =
    """on x=-20..26,y=-36..17,z=-47..7
      |on x=-20..33,y=-21..23,z=-26..28
      |on x=-22..28,y=-29..23,z=-38..16
      |on x=-46..7,y=-6..46,z=-50..-1
      |on x=-49..1,y=-3..46,z=-24..28
      |on x=2..47,y=-22..22,z=-23..27
      |on x=-27..23,y=-28..26,z=-21..29
      |on x=-39..5,y=-6..47,z=-3..44
      |on x=-30..21,y=-8..43,z=-13..34
      |on x=-22..26,y=-27..20,z=-29..19
      |off x=-48..-32,y=26..41,z=-47..-37
      |on x=-12..35,y=6..50,z=-50..-2
      |off x=-48..-32,y=-32..-16,z=-15..-5
      |on x=-18..26,y=-33..15,z=-7..46
      |off x=-40..-22,y=-38..-28,z=23..41
      |on x=-16..35,y=-41..10,z=-47..6
      |off x=-32..-23,y=11..30,z=-14..3
      |on x=-49..-5,y=-3..45,z=-29..18
      |off x=18..30,y=-20..-8,z=-3..13
      |on x=-41..9,y=-7..43,z=-33..15
      |on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
      |on x=967..23432,y=45373..81175,z=27513..53682""".stripMargin.split("\n")
}

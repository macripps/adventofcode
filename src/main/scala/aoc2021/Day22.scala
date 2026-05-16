package aoc2021

import aoc.NewDay
import aoc2021.Day22._

class Day22 extends NewDay(2021, 22) {
  part(1) {
    execute { in =>
      val steps = in.map(parse).filter(p => p.x0 >= -50 &&
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

  part(2) {
    execute { _ =>
      ""
    }
  }
}

object Day22 {
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
}

object Day22Main extends Day22

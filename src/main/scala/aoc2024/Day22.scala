package aoc2024

import aoc.NewDay

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

class Day22 extends NewDay(2024, 22) {
  part(1) {
    test(
      """1""".stripMargin -> 8685429L)
    test(
      """10""".stripMargin -> 4700978L)
    test(
      """100""" -> 15273692L)
    test(
      """2024""" -> 8667524L
    )
    test(
      """1
        |10
        |100
        |2024""".stripMargin -> 37327623L)
    execute { ls =>
      ls.par.map { l =>
        var s = l.toLong

        (1 to 2000).foreach { _ =>
          s = next(s)
        }
        s
      }.sum
    }
  }

  val mask = 0xFFFFFFL

  def next(n: Long): Long = {
    var s = n
    s = (s ^ (s << 6)) & mask
    s = (s ^ (s >> 5)) & mask
    s = (s ^ (s << 11)) & mask
    s
  }

  part(2) {
    test(
      """1
        |2
        |3
        |2024""".stripMargin -> 23)
    execute { ls =>
      val out = mutable.Map[(Short, Short, Short, Short), Long]().withDefault(_ => 0L)
      ls.foreach { l =>
        var s = l.toLong
        val seen = mutable.Set[(Short, Short, Short, Short)]()

        var v0 = s % 10.toShort
        s = next(s)
        var v1 = (s % 10).toShort
        s = next(s)
        var v2 = (s % 10).toShort
        s = next(s)
        var v3 = (s % 10).toShort
        var i0 = v0.toShort
        var i1 = (v1 - v0).toShort
        var i2 = (v2 - v1).toShort
        var i3 = (v3 - v2).toShort
        (4 to 2000).foreach { _ =>
          val t = (i0, i1, i2, i3)
          if (!seen.contains(t)) {
            seen += t
            out(t) = out(t) + (s % 10)
          }
          s = next(s)
          i0 = i1
          v0 = v1
          i1 = i2
          v1 = v2
          i2 = i3
          v2 = v3
          i3 = ((s % 10) - v3).toShort
          v3 = (s % 10).toShort
        }
      }
      out.maxBy(_._2)._2
    }
  }

}

object Day22Main extends Day22

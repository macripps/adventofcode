package aoc2025

import scala.collection.mutable

class Day11 extends aoc.NewDay(2025, 11) {

  part(1) {
    test("""aaa: you hhh
           |you: bbb ccc
           |bbb: ddd eee
           |ccc: ddd eee fff
           |ddd: ggg
           |eee: out
           |fff: out
           |ggg: out
           |hhh: ccc fff iii
           |iii: out""".stripMargin -> 5)
    execute { ls =>
      val edges = mutable.HashSet[(String, String)]()
      ls.foreach { l =>
        val Array(from, to) = l.split(": ")
        val tos = to.split(' ')
        tos.foreach { t =>
          edges.add((from, t))
        }
      }
      val start = "you"
      val cache = mutable.Map[String, Int](("out", 1))

      def searchFrom(node: String): Int = {
        if (cache.contains(node)) {
            cache(node)
        } else {
          val outs = edges.filter(_._1 == node)
          val s = outs.toList.map(o => searchFrom(o._2)).sum
          cache(node) = s
          s
        }
      }

      searchFrom(start)
    }

    part(2) {
      test("""svr: aaa bbb
             |aaa: fft
             |fft: ccc
             |bbb: tty
             |tty: ccc
             |ccc: ddd eee
             |ddd: hub
             |hub: fff
             |eee: dac
             |dac: fff
             |fff: ggg hhh
             |ggg: out
             |hhh: out""".stripMargin -> 2)
      execute { ls =>
        val edges = mutable.HashSet[(String, String)]()
        ls.foreach { l =>
          val Array(from, to) = l.split(": ")
          val tos = to.split(' ')
          tos.foreach { t =>
            edges.add((from, t))
          }
        }

        def fromTo(from: String, to: String): Long = {
          val cache = mutable.Map[String, Long]((to, 1L))

          def searchFrom(node: String): Long = {
            if (cache.contains(node)) {
              cache(node)
            } else {
              val outs = edges.filter(_._1 == node)
              val s = outs.toList.map(o => searchFrom(o._2)).sum
              cache(node) = s
              s
            }
          }

          searchFrom(from)
        }
        (fromTo("svr", "fft") * fromTo("fft", "dac") * fromTo("dac", "out")) +
          (fromTo("svr", "dac") * fromTo("dac", "fft") * fromTo("fft", "out"))
      }
    }
  }

}

object Day11Main extends Day11
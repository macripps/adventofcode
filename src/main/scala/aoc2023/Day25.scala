package aoc2023

import aoc.NewDay

import scala.collection.mutable

class Day25 extends NewDay(2023, 25) {

  part(1) {
    test {
      """jqt: rhn xhk nvd
        |rsh: frs pzl lsr
        |xhk: hfx
        |cmg: qnr nvd lhk bvb
        |rhn: xhk bvb hfx
        |bvb: xhk hfx
        |pzl: lsr hfx nvd
        |qnr: nvd
        |ntq: jqt hfx bvb xhk
        |nvd: lhk
        |lsr: lhk
        |rzs: qnr cmg lsr rsh
        |frs: qnr lhk lsr""".stripMargin -> 54
    }

    execute { input =>
      val edges = mutable.Map[String,Set[String]]().withDefaultValue(Set())
      input.foreach { line =>
        val Array(lhs, rhs) = line.split(": ")
        val rs = rhs.split(" ")
        rs.foreach { r =>
          edges(lhs) = edges(lhs) + r
          edges(r) = edges(r) + lhs
        }
      }

      if (debug()) {
        edges("hfx") = edges("hfx") - "pzl"
        edges("pzl") = edges("pzl") - "hfx"
        edges("bvb") = edges("bvb") - "cmg"
        edges("cmg") = edges("cmg") - "bvb"
        edges("nvd") = edges("nvd") - "jqt"
        edges("jqt") = edges("jqt") - "nvd"
        val hfx = reachableFrom(edges, "hfx").size
        val pzl = reachableFrom(edges, "pzl").size
        println(hfx, pzl)
        hfx * pzl
      } else {
        // Draw graph in dot, examine for three lines between connected subgraphs
        edges("njx") = edges("njx") - "pbx"
        edges("pbx") = edges("pbx") - "njx"
        edges("zvk") = edges("zvk") - "sxx"
        edges("sxx") = edges("sxx") - "zvk"
        edges("sss") = edges("sss") - "pzr"
        edges("pzr") = edges("pzr") - "sss"
        val njx = reachableFrom(edges, "njx").size
        val pbx = reachableFrom(edges, "pbx").size
        println(njx, pbx)
        njx * pbx
      }
    }
  }

  private[this] def reachableFrom(value: mutable.Map[String, Set[String]], str: String): mutable.Set[String] = {
    val reached = mutable.Set[String]()
    val toSearch = mutable.Queue[String](str)
    while (toSearch.nonEmpty) {
      val next = toSearch.dequeue()
      if (!reached.contains(next)) {
        reached.add(next)
        toSearch.addAll(value(next))
      }
    }
    reached
  }

}

object Day25Main extends Day25

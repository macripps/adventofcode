package aoc2020

object Day6 {

  def main(): Unit = {
    val lineGroups = asGroupsSeparatedByBlankLines(readFileToIterable("aoc2020/day6.input"))
    val uniqueAnswers = lineGroups.map { lineGroup =>
      lineGroup.flatMap { l => l.toSet }.toSet.size
    }.sum
    println("There were " + uniqueAnswers + " unique answers")

    val commonAnswers = lineGroups.map { lineGroup =>
      lineGroup.map { l => l.toSet }
    }.map { groups =>
      var base = "abcdefghijklmnopqrstuvwxyz".toSet
      groups.foreach { g =>
        base = base.intersect(g)
      }
      base.size
    }.sum
    println("There were " + commonAnswers + " common answers")
  }

}

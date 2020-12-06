package aoc2020

object Day6 {

  def main(): Unit = {
    val lineGroups = asGroupsSeparatedByBlankLines(readFileToIterable("aoc2020/day6.input"))
    val uniqueAnswers = lineGroups.map { lineGroup =>
      lineGroup.flatMap { l => l }.toSet.size
    }.sum
    println("There were " + uniqueAnswers + " unique answers")

    val commonAnswers = lineGroups.map { groups =>
      groups.foldLeft("abcdefghijklmnopqrstuvwxyz".toSet) { (z, i) =>
        z.intersect(i.toSet)
      }.size
    }.sum
    println("There were " + commonAnswers + " common answers")
  }

}

package aoc2020

object Day9 {

  def main(): Unit = {
    val lines = readFileToIterable("aoc2020/day9.input").toArray.map(_.toLong)
    val out = (25 until lines.length).flatMap { i =>
      val sub = lines.slice(i - 25, i)
      val target = lines(i)
      Day1.findPairThatSumTo(sub.sorted, target) match {
        case None => Some(target)
        case Some(_) => None
      }
    }
    val part1Answer = out.head
    println("There was no sum for " + part1Answer)

    var i = 0
    while (i < lines.length - 3) {
      var j = i + 2
      while (j < lines.length - 1 && lines.slice(i, j).sum < part1Answer) {
        j = j + 1
      }
      val longs = lines.slice(i, j)
      if (longs.sum == part1Answer) {
        println("The contiguous sub array from " + i + " to " + j + " adds to " + part1Answer)
        val min = longs.min
        val max = longs.max
        println("The min and max are " + min + " and " + max + " and sum to " + (min + max))
      }
      i = i + 1
    }
  }
}

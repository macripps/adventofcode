package aoc2020

object Day3 {

  def main(): Unit = {
    val lines = readFileToIterable("aoc2020/day3.input").toArray
    val slopes = Array[(Int,Int)](
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2),
    )
    var total = 1
    slopes.foreach { slope =>
      var x = 0
      var y = 0
      var trees = 0
      while (x < lines.length) {
        val pos = lines(x).charAt(y)
        if (pos == '#') {
          trees = trees + 1
        }
        y = (y + slope._1) % lines(x).length
        x = x + slope._2
      }
      println("You hit " + trees + " trees.")
      total *= trees
    }
    println("The total is " + total)
  }

}

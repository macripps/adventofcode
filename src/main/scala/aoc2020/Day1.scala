package aoc2020

object Day1 {

  def main(): Unit = {
    val input = readFileToArray("day1.input").map(l => Integer.parseInt(l)).toArray.sorted
    val pair = findPairThatSumTo(input, 2020)
    pair foreach { p =>
      println("Day1.1: The product of " + p._1 + " and " + p._2 + " is " + (p._1 * p._2))
    }
    val trio = findTripleThatSumTo(input, 2020)
    trio foreach { t =>
      println("Day1.2: The product of " + t._1 + " and " + t._2 + " and " + t._3 + " is " + (t._1 * t._2 * t._3))
    }
  }

  def findPairThatSumTo(nums: Array[Int], total: Int): Option[(Int, Int)] = {
    var lower = 0
    var upper = nums.length - 1
    while (lower < upper && nums(lower) + nums(upper) != total) {
      if (nums(lower) + nums(upper) < total) {
        lower = lower + 1
      } else {
        upper = upper - 1
      }
    }
    if (nums(lower) + nums(upper) == total) Some(nums(lower), nums(upper)) else None
  }

  def findTripleThatSumTo(nums: Array[Int], total: Int): Option[(Int, Int, Int)] = {
    if (nums.length < 3) None else {
      var lower = 0
      var upper = nums.length - 1
      var floater = 1
      while (lower < upper && nums(lower) + nums(floater) + nums(upper) != total) {
        println(lower, floater, upper)
        floater = lower + 1
        while (floater < upper && nums(lower) + nums(floater) + nums(upper) < total) {
          floater = floater + 1
        }
        if (nums(lower) + nums(floater) + nums(upper) < total) {
          lower = lower + 1
        } else {
          upper = upper - 1
        }
      }
      if (nums(lower) + nums(floater) + nums(upper) == total) {
        Some(nums(lower), nums(floater), nums(upper))
      } else {
        None
      }
    }
  }
}

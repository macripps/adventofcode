package aoc

object Strings {

  def levenshteinDistance(s1: String, s2: String): Int = {
    var v1 = Array.ofDim[Int](s2.length + 1)
    var v0 = (0 to s2.length + 1).toArray
    (0 until s1.length).foreach { i =>
      v1(0) = i + 1

      (0 until s2.length).foreach { j =>
        val delCost = v0(j + 1) + 1
        val insCost = v1(j) + 1
        val subsCost = if (s1.charAt(i) == s2.charAt(j)) {
          v0(j)
        } else {
          v0(j) + 1
        }

        v1(j+1) = math.min(delCost, math.min(insCost, subsCost))
      }

      val t = v0
      v0 = v1
      v1 = t
    }
    v0(s2.length)
  }

}

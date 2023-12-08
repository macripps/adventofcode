package aoc

object Maths {

  def quadraticSolve(a: Double, b: Double, c: Double): Iterable[Double] = {
    if (a != 0) {
      val i = b * b - 4 * a * c
      if (i < 0) {
        Seq()
      } else {
        val r1 = (-b + math.sqrt(i)) / (2.0 * a)
        val r2 = (-b - math.sqrt(i)) / (2.0 * a)
        Seq(r1, r2)
      }
    } else {
      if (b != 0) {
        Seq(-c / b)
      } else if (c != 0) {
        Seq(c)
      } else Seq()
    }
  }

  def gcd(a: Int, b: Int): Int = {
    aoc.bezoutIdentity(a.toLong, b.toLong)._3.toInt
  }

  def gcd(a: Long, b: Long): Long = {
    aoc.bezoutIdentity(a, b)._3
  }

  def lcm(values: Iterable[Long]): Long = {
    values.reduce { (l: Long, r: Long) => (l * r) / gcd(l, r) }
  }
}

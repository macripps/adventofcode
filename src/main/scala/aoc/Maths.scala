package aoc

import scala.collection.mutable

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

  def primeFactors(l: Long): Seq[Long] = {
    var n = l
    val out = mutable.Buffer[Long]()
    while (n % 2 == 0) {
      n = n / 2
      out += 2L
    }
    var test = 3L
    while (test < Math.sqrt(n)) {
      while (n % test == 0) {
        n = n / test
        out += test
      }
      test = test + 2L
    }
    if (n != 1) {
      out += n
    }
    out.toSeq
  }
}

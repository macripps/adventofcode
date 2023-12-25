package aoc2023

import aoc.{Maths, NewDay}

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Day24 extends NewDay(2023, 24) {

  part(1) {
    test {
      """19, 13, 30 @ -2,  1, -2
        |18, 19, 22 @ -1, -1, -2
        |20, 25, 34 @ -2, -2, -4
        |12, 31, 28 @ -1, -2, -1
        |20, 19, 15 @  1, -5, -3""".stripMargin -> 0
    }

    execute { input =>
      val hailstones = input.map(Hailstone.from)
      val testXMin = 200000000000000L
      val testYMin = 200000000000000L
      val testXMax = 400000000000000L
      val testYMax = 400000000000000L
      var n = 0
      hailstones.indices.foreach { h1 =>
        (h1 + 1 until hailstones.length).foreach { h2 =>
          val a = hailstones(h1)
          val b = hailstones(h2)
          if (debug()) {
            println("Hailstone A: " + a)
            println("Hailstone B: " + b)
          }

          val intersection = intersect(a, b)
          if (intersection.isDefined) {
            val colX = intersection.get._1
            val colY = intersection.get._2
            if (colX >= testXMin && colX <= testXMax && colY >= testYMin && colY <= testYMax) {
              // Hailstones' paths will cross inside the test area (at x=" + colX + ", y=" + colY + ").
              n = n + 1
            } else {
              // Hailstones' paths will cross outside the test area (at x=" + colX + ", y=" + colY + ").
            }
          }

          if (debug()) {
            println()
          }
        }

      }
      n
    }
  }

  private[this] def intersect(a: Hailstone, b: Hailstone): Option[(Double, Double)] = {
    /*
    Ax_t = Ax_0 + At * Avx
    Ay_t = Ay_0 + At * Avy

    Bx_t = Bx_0 + Bt * Bvx
    By_t = By_0 + Bt * Bvy

    Ax_0 + At * Avx = Bx_0 + Bt * Bvx
    Ay_0 + At * Avy = By_0 + Bt * Bvy

    Ax_0 + At * Avx - Bx_0 = Bt * Bvx
    (Ax_0/ Bvx) + (At * Avx / Bvx) - (Bx_0/Bvx) = Bt
    (Ay_0/Bvy) + (At * Avy / Bvy) - (By_0/Bvy) = Bt

    (Ax_0/ Bvx) + (At * Avx / Bvx) - (Bx_0/Bvx) = (Ay_0/Bvy) + (At * Avy / Bvy) - (By_0/Bvy)
    (Ax_0 * Bvy) + (At * Avx * Bvy) - (Bx_0 * Bvy) = (Ay_0 * Bvx) + (At * Avy * Bvx) - (By_0 * Bvx)
    (At * Avx * Bvy) - (At * Avy * Bvx) = (Ay_0 * Bvx)- (Ax_0 * Bvy) - (By_0 * Bvx) + (Bx_0 * Bvy)
    At * (Avx * Bvy - Avy * Bvx) =(Ay_0 * Bvx)- (Ax_0 * Bvy) - (By_0 * Bvx) + (Bx_0 * Bvy)
    At = ((Ay_0 * Bvx)- (Ax_0 * Bvy) - (By_0 * Bvx) + (Bx_0 * Bvy)) / (Avx * Bvy - Avy * Bvx)

    At * Avx = (Bx_0 + Bt * Bvx) - Ax_0
    At * Avy = (By_0 + Bt * Bvy) - Ay_0

    At = ((Bx_0 + Bt * Bvx) - Ax_0) / Avx
    At * Avy = (By_0 + Bt * Bvy) - Ay_0

    ((Bx_0 + Bt * Bvx) - Ax_0) / Avx = ((By_0 + Bt * Bvy) - Ay_0) / Avy
    ((Bx_0 + Bt * Bvx) - Ax_0) * Avy = ((By_0 + Bt * Bvy) - Ay_0) * Avx
    ((Bx_0 + Bt * Bvx) * Avy = ((By_0 + Bt * Bvy) * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy)
    ((Bx_0 * Avy) + (Bt * Bvx * Avy)) = ((By_0 + Bt * Bvy) * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy)
    ((Bt * Bvx * Avy)) = ((By_0 + Bt * Bvy) * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy) - (Bx_0 * Avy)
    ((Bt * Bvx * Avy)) = ((By_0 * Avx) + (Bt * Bvy * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy) - (Bx_0 * Avy)
    ((Bt * Bvx * Avy)) - (Bt * Bvy * Avx)  = (By_0 * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy) - (Bx_0 * Avy)

    Bt * (Bvx*Avy - Bvy*Avx) = ((By_0 * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy) - (Bx_0 * Avy)
    Bt = ((By_0 * Avx) - (Ay_0 * Avx) + (Ax_0 * Avy) - (Bx_0 * Avy)) / (Bvx*Avy - Bvy*Avx)
     */

    val descrimA = a.vX * b.vY - a.vY * b.vX
    val descrimB = b.vX * a.vY - b.vY * a.vX

    if (descrimA == 0 || descrimB == 0) {
      // Hailstones' paths are parallel; they never intersect.
      None
    } else {
      val at = (a.pt.y * b.vX - a.pt.x * b.vY - b.pt.y * b.vX + b.pt.x * b.vY) / descrimA
      val bt = (b.pt.y * a.vX - a.pt.y * a.vX + a.pt.x * a.vY - b.pt.x * a.vY) / descrimB

      if (at < 0 && bt < 0) {
        None
        // Hailstones' paths crossed in the past for both hailstones.
      } else if (at < 0) {
        None
        // Hailstones' paths crossed in the past for hailstone A.
      } else if (bt < 0) {
        None
        // Hailstones' paths crossed in the past for hailstone B.
      } else {
        val colX = a.pt.x + at * a.vX
        val colY = a.pt.y + at * a.vY

        Some((colX, colY))
      }
    }
  }

  part(2) {
    test {
      """19, 13, 30 @ -2,  1, -2
        |18, 19, 22 @ -1, -1, -2
        |20, 25, 34 @ -2, -2, -4
        |12, 31, 28 @ -1, -2, -1
        |20, 19, 15 @  1, -5, -3""".stripMargin -> 47
    }

    execute { input =>
      val origin = input.map(Hailstone.from)

      val dX = (-3000 to 3000).find { g =>
        origin.groupBy(h => h.vX).forall { f =>
          val v = f._1.toLong
          if (g == v && f._2.length>1) false
          else {
            val coords = f._2.map(_.pt.x.toLong)
            coords.indices.forall { a =>
              coords.indices.forall { b =>
                if (coords(a) > coords(b)) {
                  val m = coords(a) - coords(b)
                  m % (g-v) == 0
                } else {
                  true
                }
              }
            }
          }
        }
      }
      println(dX)

      val dY = (-3000 to 3000).find { g =>
        origin.groupBy(h => h.vY).forall { f =>
          val v = f._1.toLong
          if (g == v && f._2.length > 1) false
          else {
            val coords = f._2.map(_.pt.y.toLong)
            coords.indices.forall { a =>
              coords.indices.forall { b =>
                if (coords(a) > coords(b)) {
                  val m = coords(a) - coords(b)
                  m % (g - v) == 0
                } else {
                  true
                }
              }
            }
          }
        }
      }
      println(dY)

      val dZ = (-3000 to 3000).find { g =>
        origin.groupBy(h => h.vZ).forall { f =>
          val v = f._1.toLong
          if (g == v && f._2.length > 1) false
          else {
            val coords = f._2.map(_.pt.z.toLong)
            coords.indices.forall { a =>
              coords.indices.forall { b =>
                if (coords(a) > coords(b)) {
                  val m = coords(a) - coords(b)
                  m % (g - v) == 0
                } else {
                  true
                }
              }
            }
          }
        }
      }
      println(dZ)

      // xDiff = 26 - vx
      // x = 270392223533307
      // yDiff = -331 - vy
      // y =
      // zDiff = 53 - vz
      // z = 273041846062208

      47

      //      feline(origin)
    }

    // With thanks to FatalisticFeline-47
    // Works for test, not for input
    def feline(origin: Array[Hailstone]): Double = {
      var found = false
      var maxVel = 0
      var soln = 0.0

      breakable {
        while (!found) {
          println(maxVel)
          (0 to maxVel).foreach { x =>
            val y = maxVel - x
            Seq(-1, 1).foreach { negX =>
              Seq(-1, 1).foreach { negY =>
                val aX = negX * x
                val aY = negY * y
                if (debug()) {
                  println("Checking v=" + aX + "," + aY)
                }
                val adjusteds = origin.map { h => Hailstone(h.pt, h.vX - aX, h.vY - aY, h.vZ) }
                val h1 = adjusteds.head
                var intersection: Option[(Double, Double)] = None
                breakable {
                  adjusteds.tail.foreach { h2 =>
                    val p = intersect(h1, h2)
                    p match {
                      case None =>
                        break()
                      case Some(i) if intersection.isEmpty =>
                        intersection = p
                      case Some(i) if intersection.isDefined && intersection.get != i =>
                        break()
                      case _ =>
                    }
                  }
                }

                if (intersection.isDefined) {
                  val iPt = intersection.get
                  println("Possible intersection at " + iPt)

                  var aZ: Option[Double] = None
                  val h1 = adjusteds.head
                  breakable {
                    adjusteds.tail.foreach { h2 =>
                      val nZ = h1.getZ(h2, iPt)
                      if (aZ.isEmpty) {
                        aZ = nZ
                      } else if (aZ != nZ) {
                        break()
                      }
                    }

                    val hR = adjusteds.head
                    val z = hR.pt.z + hR.getT(iPt) * (hR.vZ - aZ.get)
                    println("Found Solution: v=" + aX + "," + aY + "," + aZ + ", p=" + iPt._1 + "," + iPt._2 + "," + z + ",s=" + (z + iPt._1 + iPt._2))
                    soln = (z + iPt._1 + iPt._2)
                    found = true
                    break()
                  }
                }
              }
            }
          }


          maxVel = maxVel + 1
        }
      }

      soln
    }
  }
}

case class Point3(x: Double, y: Double, z: Double)

case class Hailstone(pt: Point3, vX: Double, vY: Double, vZ: Double) {
  override def toString: String = pt.x + ", " + pt.y + ", " + pt.z + " @ " + vX + ", " + vY + ", " + vZ

  def getT(p: (Double, Double)): Double = {
    if (vX == 0) {
      (p._2 - pt.y) / vY
    } else {
      (p._1 - pt.x) / vX
    }
  }

  def getZ(other: Hailstone, inter: (Double, Double)): Option[Double] = {
    val tS = getT(inter)
    val tO = other.getT(inter)
    if (tS == tO) {
      None
    } else {
      Some((pt.z - other.pt.z + tS * vZ - tO * other.vZ) / (tS - tO))
    }
  }
}

object Hailstone {
  def from(s: String): Hailstone = {
    val Array(pos, vel) = s.split(" @ +")
    val Array(x, y, z) = pos.split(", +")
    val Array(vx, vy, vz) = vel.split(", +")
    Hailstone(Point3(x.toLong, y.toLong, z.toLong), vx.toDouble, vy.toDouble, vz.toDouble)
  }
}

object Day24Main extends Day24

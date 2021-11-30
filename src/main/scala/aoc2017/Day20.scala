package aoc2017

import aoc.{Day, ThreePoint}

import scala.collection.mutable
import scala.util.matching.Regex

class Day20 extends Day(2017, 20) {

  import Day20._

  override def part1(input: Array[String]): String = {
    var particles = input.zipWithIndex.map {
      case (line(px: String, py: String, pz: String, vx: String, vy: String, vz: String, ax: String, ay: String, az: String), idx: Int) =>
        Particle(idx, ThreePoint(px.toDouble, py.toDouble, pz.toDouble), (vx.toDouble, vy.toDouble, vz.toDouble), (ax.toDouble, ay.toDouble, az.toDouble))
    }
    val t = 1_000_000L
    particles = particles.map { p =>
      val pn = p.at(t)
      pn
    }
    particles.minBy { p =>
      p.position.manhattanDistanceTo(ThreePoint(0, 0, 0))
    }.idx.toString
  }

  val example: Array[String] =
    """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
      |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
      |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
      |p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>""".stripMargin.split("\n")

  override def part2(input: Array[String]): String = {
    val particles = input.zipWithIndex.map {
      case (line(px: String, py: String, pz: String, vx: String, vy: String, vz: String, ax: String, ay: String, az: String), idx: Int) =>
        Particle(idx, ThreePoint(px.toDouble, py.toDouble, pz.toDouble), (vx.toDouble, vy.toDouble, vz.toDouble), (ax.toDouble, ay.toDouble, az.toDouble))
    }

    var parts = mutable.Buffer[Particle](particles:_*)
    (1 to 500).foreach { _ =>
      parts = parts.map(_.step)
      parts = parts.filter { p => !parts.exists { p2 => p.idx != p2.idx && p.position == p2.position } }
    }
    parts.size.toString
  }
}

object Day20 {
  def apply() = new Day20()

  val line: Regex = raw"p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>".r

  case class Particle(idx: Long, position: ThreePoint, velocity: (Double, Double, Double), acc: (Double, Double, Double)) {
    def vel(t: Long): (Double, Double, Double) = (velocity._1 + t * acc._1, velocity._2 + t * acc._2, velocity._3 + t * acc._3)

    def pos(t: Long): ThreePoint = ThreePoint(
      position.x + t * (velocity._1 + t * (acc._1 / 2)),
      position.y + t * (velocity._2 + t * (acc._2 / 2)),
      position.z + t * (velocity._3 + t * (acc._3 / 2)),
    )

    def at(t: Long): Particle = copy(position = pos(t), velocity = vel(t))

    def step: Particle = {
      val vn = (velocity._1 + acc._1, velocity._2 + acc._2, velocity._3 + acc._3)
      val pn = ThreePoint(position.x + vn._1, position.y + vn._2, position.z + vn._3)
      copy(position = pn, velocity = vn)
    }
  }

}

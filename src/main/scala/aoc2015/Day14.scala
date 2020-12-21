package aoc2015

import aoc.Day
import Day14._

import scala.collection.mutable

class Day14 extends Day(2015, 14) {
  val time = 2503
  override def part1: String = {
    input.map {
      case reindeer(name, speed, flyingTime, restingTime) => {
        (name, distance(speed.toInt, flyingTime.toInt, restingTime.toInt, time.toInt))
      }
    }.maxBy(_._2).toString()
  }

  override def part2: String = {
    val reindeers = input.map {
      case reindeer(name, speed, flyingTime, restingTime) =>
        (name, speed.toInt, flyingTime.toInt, restingTime.toInt)
    }
    val reindeerPoints = mutable.Map[String, Int]()
    (1 to time).foreach { t =>
      val distanceEach = reindeers.map { r =>
        r._1 -> distance(r._2, r._3, r._4, t)
      }
      val sorted = distanceEach.sortBy(-_._2)
      distanceEach.filter(_._2 == sorted.head._2).foreach { r =>
        if (reindeerPoints.contains(r._1)) {
          reindeerPoints(r._1) = reindeerPoints(r._1) + 1
        } else {
          reindeerPoints(r._1) = 1
        }
      }
    }
    reindeerPoints.maxBy(_._2).toString()
  }
}

object Day14 {
  def apply() = new Day14()

  val reindeer = raw"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.".r

  def distance(speed: Int, flyingTime: Int, restingTime: Int, atTime: Int): Int = {
    val numberOfFlights = atTime / (flyingTime + restingTime)
    val flyingTimeLeft = math.min(flyingTime, atTime % (flyingTime + restingTime))
    (numberOfFlights * flyingTime * speed.toInt) + (flyingTimeLeft * speed.toInt)
  }
}

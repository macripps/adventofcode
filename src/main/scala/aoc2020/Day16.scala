package aoc2020

import aoc.Day
import aoc2020.Day16._

class Day16 extends Day(2020, 16) {
  override def part1(input: Array[String]): String = {
    val ranges = input.take(20).flatMap(parseRule)
    val errorRate = input.drop(25).map { ticket =>
      val entries = ticket.split(",").map(_.toInt)
      entries.map { e =>
        ranges.find(r => r.contains(e)) match {
          case Some(_) => 0
          case None => e
        }
      }.sum
    }.sum
    "Ticket scanning error rate is " + errorRate
  }

  override def part2(input: Array[String]): String = {
    var ranges = input.take(20).map(l => l.substring(0, l.indexOf(':')) -> parseRule(l)).toMap
    val validTickets = input.drop(25).map { ticket =>
      ticket.split(",").map(_.toInt)
    }.filter { ticket =>
      Day16.valid(ticket, ranges.values.flatten.toArray)
    }
    val valuesByField = validTickets.transpose

    val yourTicket = input.drop(22).head.split(",")

    var product = 1L
    while (ranges.nonEmpty) {
      valuesByField.indices.foreach { i =>
        var acceptableRanges = Seq[String]()
        val field = valuesByField(i)
        ranges.foreach { case f -> rs =>
          val range1 = rs(0)
          val range2 = rs(1)
          if (field.forall { f =>
            range1.contains(f) || range2.contains(f)
          }) {
            acceptableRanges = acceptableRanges :+ f
          }
        }
        if (acceptableRanges.length == 1) {
          val fieldName = acceptableRanges.head
          ranges = ranges - fieldName
          if (Seq("departure location", "departure station", "departure platform", "departure track", "departure date", "departure time").contains(fieldName)) {
            product *= yourTicket(i).toLong
          }
        }
      }
    }
    "The product is " + product
  }

}

object Day16 {
  def apply() = new Day16()

  def parseRule(line: String): Array[Range] = {
    val fieldRanges = line.split(": ")
    val ranges = fieldRanges(1).split(" or ")
    ranges.map(_.split("-")).map(r => Range.inclusive(r(0).toInt, r(1).toInt))
  }

  def valid(ticket: Iterable[Int], ranges: Array[Range]): Boolean = {
    ticket.forall { e =>
      ranges.find(r => r.contains(e)) match {
        case Some(_) => true
        case None => false
      }
    }
  }
}

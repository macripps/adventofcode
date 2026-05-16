package aoc2022

import aoc.NewDay

import scala.collection.mutable

class Day13 extends NewDay(2022, 13) {
  import Day13._

  part(1) {
    execute { in =>
      aoc.asGroupsSeparatedByBlankLines(in).zipWithIndex.map { case (pairs, idx) =>
        val left = parsePacket(pairs.head)
        val right = parsePacket(pairs.tail.head)
        val result: Int = PacketOrdering.compare(left, right)
        if (result == -1) idx + 1 else 0
      }.sum
    }
  }

  part(2) {
    execute { in =>
      val divider2 = PacketList(Seq(PacketList(Seq(PacketNumber(2)))))
      val divider6 = PacketList(Seq(PacketList(Seq(PacketNumber(6)))))
      val allPackets = in.filter(_.nonEmpty).map(parsePacket) ++ Seq(
        divider2, divider6
      )
      val sortedPackets = allPackets.sorted(PacketOrdering)
      (sortedPackets.indexOf(divider2) + 1) * (sortedPackets.indexOf(divider6) + 1)
    }
  }

  private[this] def parsePacket(in: String): Packet = {
    if (in.isEmpty) {
      PacketList(Seq.empty)
    } else if (in.charAt(0) == '[') {
      // parseList
      var depth = 1
      var start = 1
      var cursor = 1
      val items = mutable.Buffer[Packet]()
      while (depth > 0) {
        if (in.charAt(cursor) == '[') {
          depth = depth + 1
        } else if (in.charAt(cursor) == ']') {
          depth = depth - 1
        } else if (in.charAt(cursor) == ',' && depth == 1) {
          items.append(parsePacket(in.substring(start, cursor)))
          start = cursor + 1
        }
        cursor = cursor + 1
      }
      if (start != cursor - 1) {
        items.append(parsePacket(in.substring(start, cursor - 1)))
      }
      PacketList(items.toSeq)
    } else if (in.indexOf(',') == -1) {
      PacketNumber(in.toInt)
    } else {
      throw new IllegalArgumentException(in)
    }
  }
}

object Day13Main extends Day13

object Day13 {
}

object PacketOrdering extends Ordering[Packet] {
  override def compare(left: Packet, right: Packet): Int = {
    if (left.isInstanceOf[PacketNumber] && right.isInstanceOf[PacketNumber]) {
      val leftN = left.asInstanceOf[PacketNumber]
      val rightN = right.asInstanceOf[PacketNumber]
      compareNumbers(leftN, rightN)
    } else if (left.isInstanceOf[PacketNumber] && right.isInstanceOf[PacketList]) {
      val v = PacketList(Seq(left))
      compare(v, right)
    } else if (left.isInstanceOf[PacketList] && right.isInstanceOf[PacketNumber]) {
      val v = PacketList(Seq(right))
      compare(left, v)
    } else {
      val leftL = left.asInstanceOf[PacketList]
      val rightL = right.asInstanceOf[PacketList]
      compareLists(leftL, rightL)
    }
  }

  private[this] def compareNumbers(l: PacketNumber, r: PacketNumber): Int = {
    if (l.data < r.data) {
      -1
    } else if (l.data > r.data) {
      1
    } else {
      0
    }
  }

  private[this] def compareLists(l: PacketList, r: PacketList): Int = {
    if (l.data.isEmpty && r.data.isEmpty) {
      0
    } else if (l.data.isEmpty && r.data.nonEmpty) {
      -1
    } else if (l.data.nonEmpty && r.data.isEmpty) {
      1
    } else {
      val left = l.data.head
      val right = r.data.head
      val result = compare(left, right)
      if (result != 0) {
        result
      } else {
        compareLists(PacketList(l.data.tail), PacketList(r.data.tail))
      }
    }
  }
}

sealed trait Packet {
}

case class PacketList(val data: Seq[Packet]) extends Packet {
  override def toString: String = data.mkString("[", ",", "]")
}

case class PacketNumber(val data: Int) extends Packet {
  override def toString: String = data.toString
}

package aoc2021

import aoc.Day
import aoc2021.Day16._

class Day16 extends Day(2021, 16) {
  override def part1(input: Array[String]): String = {
    val packet = hexStringToBinary(input.head)
    val (p, _) = parse(packet, 0)
    println(p)
    sumOfVersions(p).toString
  }

  def sumOfVersions(p: Packet): Int = {
    p match {
      case Literal(v, _, _) => v
      case Operator(v, _, _, _, pkts) => v + pkts.map(sumOfVersions).sum
      case _ => 0
    }
  }

  override def part2(input: Array[String]): String = {
    val packet = hexStringToBinary(input.head)
    val (p, _) = parse(packet, 0)
    eval(p).toString
  }

  def eval(p: Packet): Long = {
    p match {
      case Literal(_, _, v) => v
      case Operator(_, v, _, _, ps) => v match {
        case 0 => ps.map(eval).sum
        case 1 => ps.map(eval).product
        case 2 => ps.map(eval).min
        case 3 => ps.map(eval).max
        case 5 => if (eval(ps.head) > eval(ps.tail.head)) 1 else 0
        case 6 => if (eval(ps.head) < eval(ps.tail.head)) 1 else 0
        case 7 => if (eval(ps.head) == eval(ps.tail.head)) 1 else 0
      }
    }
  }
}

object Day16 {

  val example0 = "D2FE28"
  val example1 = "8A004A801A8002F478"
  val example2 = "620080001611562C8802118E34"
  val example3 = "C0015000016115A2E0802F182340"
  val example4 = "A0016C880162017C3686B18A3D4780"

  val example5 = "C200B40A82"

  def apply() = new Day16

  trait Packet

  case class Literal(version: Int, typeId: Int, value: Long) extends Packet

  case class Operator(version: Int, typeId: Int, lengthTypeId: Int, length: Int, subPacket: Seq[Packet]) extends Packet

  val hexToBinary = Map[Char, String](
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111",
  )

  def hexStringToBinary(input: String): String = {
    input.toCharArray.map(hexToBinary).mkString
  }

  def parse(input: String, start: Int): (Packet, Int) = {
    var pos = start
    val version = Integer.parseInt(input.substring(pos, pos + 3), 2)
    pos = pos + 3
    val typeId = Integer.parseInt(input.substring(pos, pos + 3), 2)
    pos = pos + 3
    typeId match {
      case 4 =>
        var value = ""
        var nextPart = ""
        do {
          nextPart = input.substring(pos, pos + 5)
          pos = pos + 5
          value = value + nextPart.tail
        } while (nextPart.head != '0')
        val v = java.lang.Long.parseLong(value, 2)
        (Literal(version, typeId, v), pos)
      case _ =>
        val lengthTypeId = input.charAt(pos) - '0'
        pos = pos + 1
        var length = 0
        val subPackets = lengthTypeId match {
          case 0 =>
            length = Integer.parseInt(input.substring(pos, pos + 15), 2)
            pos = pos + 15
            var packets = Seq[Packet]()
            val end = pos + length
            while (pos < end) {
              val (packet, newPos) = parse(input, pos)
              pos = newPos
              packets = packets :+ packet
            }
            packets
          case 1 =>
            length = Integer.parseInt(input.substring(pos, pos + 11), 2)
            pos = pos + 11
            (1 to length).map { _ =>
              val (packet, newPos) = parse(input, pos)
              pos = newPos
              packet
            }
        }
        (Operator(version, typeId, lengthTypeId, length, subPackets), pos)
    }
  }
}

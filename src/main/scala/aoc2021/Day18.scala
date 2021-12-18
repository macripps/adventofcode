package aoc2021

import aoc.Day
import aoc2021.Day18._

import scala.annotation.tailrec

class Day18 extends Day(2021, 18) {
  override def part1(input: Array[String]): String = {
    val o = fullSum(input)
    magnitude(o).toString
  }

  val example = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
                  |[[[5,[2,8]],4],[5,[[9,9],0]]]
                  |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
                  |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
                  |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
                  |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
                  |[[[[5,4],[7,7]],8],[[8,3],8]]
                  |[[9,3],[[9,9],[6,[4,9]]]]
                  |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
                  |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.split("\n")

  override def part2(input: Array[String]): String = {
    input.flatMap { a =>
      input.map { b =>
        if (a != b) {
          magnitude(snailfishSum(parse(a), parse(b)))
        } else Int.MinValue
      }
    }.max.toString
  }
}

object Day18 {
  def apply() = new Day18

  case class SnailfishNumber() {
    var left: SnailfishNumber = _
    var right: SnailfishNumber = _
    var data: Option[Int] = None
    var lNeighbour: SnailfishNumber = _
    var rNeighbour: SnailfishNumber = _

    override def toString: String = {
      if (left == null && right == null) {
        data.get.toString
      } else "[" + left.toString + "," + right.toString + "]"
    }
  }

  def magnitude(s: SnailfishNumber): Int = {
    if (s.data.isDefined) {
      s.data.get
    } else {
      3 * magnitude(s.left) + 2 * magnitude(s.right)
    }
  }

  def reduce(x: SnailfishNumber): SnailfishNumber = {
    var changed = true
    while (changed) {
      changed = explode(x)
      if (!changed) {
        changed = split(x)
      }
    }
    x
  }

  def explode(number: SnailfishNumber): Boolean = {
    _explode(number, 4)
  }

  def _explode(number: SnailfishNumber, depth: Int): Boolean = {
    if (number.data.isEmpty) {
      if (depth == 0) {
        number.data = Some(0)
        if (number.left.lNeighbour != null) {
          number.left.lNeighbour.data = Some(number.left.lNeighbour.data.get + number.left.data.get)
          number.lNeighbour = number.left.lNeighbour
          number.lNeighbour.rNeighbour = number
        }
        if (number.right.rNeighbour != null) {
          number.right.rNeighbour.data = Some(number.right.rNeighbour.data.get + number.right.data.get)
          number.rNeighbour = number.right.rNeighbour
          number.rNeighbour.lNeighbour = number
        }
        number.left = null
        number.right = null
        true
      } else {
        _explode(number.left, depth - 1) || _explode(number.right, depth - 1)
      }
    } else {
      false
    }
  }

  @tailrec
  def leftmost(n: SnailfishNumber): SnailfishNumber = {
    if (n.data.isDefined) {
      n
    } else {
      leftmost(n.left)
    }
  }

  @tailrec
  def rightmost(n: SnailfishNumber): SnailfishNumber = {
    if (n.data.isDefined) {
      n
    } else {
      rightmost(n.right)
    }
  }

  def split(number: SnailfishNumber): Boolean = {
    if (number.data.nonEmpty && number.data.get >= 10) {
      val left = SnailfishNumber()
      left.data = Some(number.data.get / 2)
      val right = SnailfishNumber()
      right.data = Some((number.data.get + 1) / 2)
      left.rNeighbour = right
      right.lNeighbour = left
      number.left = left
      number.right = right
      number.data = None
      if (number.lNeighbour != null) {
        left.lNeighbour = number.lNeighbour
        left.lNeighbour.rNeighbour = left
        number.lNeighbour = null
      }
      if (number.rNeighbour != null) {
        right.rNeighbour = number.rNeighbour
        right.rNeighbour.lNeighbour = right
        number.rNeighbour = null
      }
      true
    } else if (number.data.isEmpty) {
      if (!split(number.left)) {
        split(number.right)
      } else {
        true
      }
    } else {
      false
    }
  }

  def snailfishSum(x: SnailfishNumber, y: SnailfishNumber): SnailfishNumber = {
    val newNumber = SnailfishNumber()
    rightmost(x).rNeighbour = leftmost(y)
    leftmost(y).lNeighbour = rightmost(x)
    newNumber.left = x
    newNumber.right = y
    reduce(newNumber)
  }

  def parse(l: String): SnailfishNumber = {
    if (l.charAt(0) == '[') {
      var depth = 1
      var index = 1
      var middle = Int.MinValue
      while (depth != 0) {
        if (l.charAt(index) == ',' && depth == 1) {
          middle = index
        }
        if (l.charAt(index) == '[') {
          depth = depth + 1
        }
        if (l.charAt(index) == ']') {
          depth = depth - 1
        }
        index = index + 1
      }
      val left = parse(l.slice(1, middle))
      val right = parse(l.substring(middle + 1).init)
      rightmost(left).rNeighbour = leftmost(right)
      leftmost(right).lNeighbour = rightmost(left)
      val out = SnailfishNumber()
      out.left = left
      out.right = right
      out
    } else {
      val out = SnailfishNumber()
      out.data = Some(l.toInt)
      out
    }
  }

  def fullSum(ls: Array[String]): SnailfishNumber = {
    ls.map(parse).reduce(snailfishSum)
  }
}

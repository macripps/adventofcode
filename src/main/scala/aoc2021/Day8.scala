package aoc2021

import aoc.Day

class Day8 extends Day(2021, 8) {
  override def part1(input: Array[String]): String = {
    input.map { l =>
      val lr = l.split(" \\| ")
      val outputNs = lr(1).split(' ')
      outputNs.count(l => l.length == 2 || l.length == 3 || l.length == 4 || l.length == 7)
    }.sum.toString
  }

  override def part2(input: Array[String]): String = {
    input.map { l =>
      val lr = l.split(" \\| ")
      val inputNs = lr(0).split(' ')
      val outputNs = lr(1).split(' ')
      val one = (inputNs ++ outputNs).find(_.length == 2).head
      val seven = (inputNs ++ outputNs).find(_.length == 3).head
      val eight = (inputNs ++ outputNs).find(_.length == 7).head
      val four = (inputNs ++ outputNs).find(_.length == 4).head
      val zero = (inputNs ++ outputNs).find(x => x.length == 6 && x.diff(one).length == 4 && x.diff(four).length == 3).head
      val nine = (inputNs ++ outputNs).find(x => x.length == 6 && x.diff(four).length == 2).head
      val two = (inputNs ++ outputNs).find(x => x.length == 5 && x.diff(seven).length == 3 && x.diff(one).length == 4 && x.diff(four).length == 3).head
      val six = (inputNs ++ outputNs).find(x => x.length == 6 && x.diff(one).length == 5).head
      val five = (inputNs ++ outputNs).find(x => x.length == 5 && x.diff(two).length == 2).head
//      val three= (inputNs ++ outputNs).find(x => x.length == 5 && x.diff(one).length == 3).head
      val topSegment = seven.diff(one)
      val topRightSegment = nine.diff(six)
      val bottomRightSegment = one.diff(topRightSegment)
      val bottomSegment = nine.diff(four).diff(seven)
      val bottomLeftSegment = six.diff(nine)
      val topLeftSegment = five.diff(two).diff(one)
      val centerSegment = eight.diff(zero)
      outputNs.map { n =>
        if (n.length == 2) 1 else
          if (n.length == 4) 4 else
            if (n.length == 3) 7 else
              if (n.length == 7) 8 else
                if (n.length == 5 && n.contains(topLeftSegment)) 5 else
                  if (n.length == 5 && n.contains(bottomLeftSegment)) 2 else
                    if (n.length == 5) 3 else
                      if (n.length == 6 && n.contains(centerSegment) && n.contains(bottomLeftSegment)) 6 else
                        if (n.length == 6 && n.contains(centerSegment)) 9 else
                          0
      }.mkString.toInt
    }.sum.toString
  }
}

object Day8 {
  def apply() = new Day8
}

package aoc2020

import aoc.Day
import aoc2020.Day20._

class Day20 extends Day {
  override def year: Int = 2020

  override def day: Int = 20

  override def part1(input: Array[String]): String = {
    val tiles = asGroupsSeparatedByBlankLines(input.dropRight(1)).map { tileLines =>
      val tileHeader = tileLines.head
      val id = tileHeader.drop(5).dropRight(1).toLong

      val tile = tileLines.tail.toArray.map(_.toCharArray)
      val transpose = tile.transpose

      val contents = tile.drop(1).dropRight(1).map(_.drop(1).dropRight(1))
      id -> Tile(id, tile.head.mkString(""), tile.last.mkString(""), transpose.head.mkString(""), transpose.last.mkString(""))(contents)
    }.toMap

    val grid = Array.ofDim[Option[Tile]](12, 12)
    grid.indices.foreach { r =>
      grid(r).indices.foreach { c =>
        grid(r)(c) = None
      }
    }

    grid(0)(0) = Some(tiles(2801).rotate.rotate)
    grid(1)(1) = Some(tiles(1093).flipH.rotate.rotate)
    grid(11)(11) = Some(tiles(2719))
    fill(grid, tiles - 2801 - 1093 - 2719)
    println(grid.map(r => r.map { c => c.map(_.id).getOrElse(0) }.mkString(" ")).mkString("\n"))
    ""
  }

  override def part2(input: Array[String]): String = {
    val tileGroups = asGroupsSeparatedByBlankLines(input.dropRight(1))
    val tiles = tileGroups.map { tileLines =>
      val tileHeader = tileLines.head
      val id = tileHeader.drop(5).dropRight(1).toLong

      val tile = tileLines.tail.toArray.map(_.toCharArray)
      val transpose = tile.transpose

      val contents = tile.drop(1).dropRight(1).map(_.drop(1).dropRight(1))

      id -> Tile(id, tile.head.mkString(""), tile.last.mkString(""), transpose.head.mkString(""), transpose.last.mkString(""))(contents)
    }.toMap
    val grid = Array.ofDim[Option[Tile]](12, 12)
    grid.indices.foreach { r =>
      grid(r).indices.foreach { c =>
        grid(r)(c) = None
      }
    }

    grid(0)(0) = Some(tiles(2801).rotate.rotate)
    grid(1)(1) = Some(tiles(1093).flipH.rotate.rotate)
    grid(11)(11) = Some(tiles(2719))
    fill(grid, tiles - 2801 - 1093 - 2719)

    val out = Array.fill(96)("")

    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        val tile = grid(y)(x).get
        (0 until 8).foreach { r =>
          out(8 * y + r) = out(8 * y + r) + tile.contents(r).mkString("")
        }
      }
    }
    val outputGrid = out.map(_.toCharArray)

    val monster =
      """                  #
        |#    ##    ##    ###
        | #  #  #  #  #  #   """.stripMargin.split("\n").map(_.toCharArray)

    val megaTile = Tile(0, "x........", ".##...#..#", "abcdefghij", "klmnopqrst")(outputGrid).flipV.rotate

    val c = megaTile.contents
    (0 until c.length - 3).foreach { y =>
      (0 until c(y).length - 20).foreach { x =>
        if ((c(y + 1)(x) == '#' || c(y+1)(x) == 'Z') &&
          (c(y + 2)(x + 1) == '#' || c(y+2)(x+1) == 'Z') &&
          (c(y + 2)(x + 4) == '#' || c(y+2)(x+4) == 'Z') &&
          (c(y + 1)(x + 5) == '#' || c(y+1)(x+5) == 'Z') &&
          (c(y + 1)(x + 6) == '#' || c(y+1)(x+6) == 'Z') &&
          (c(y + 2)(x + 7) == '#' || c(y+2)(x+7) == 'Z') &&
          (c(y + 2)(x + 10) == '#' || c(y+2)(x+10) == 'Z') &&
          (c(y + 1)(x + 11) == '#' || c(y+1)(x+11) == 'Z') &&
          (c(y + 1)(x + 12) == '#' || c(y+1)(x+12) == 'Z') &&
          (c(y + 2)(x + 13) == '#' || c(y+2)(x+13) == 'Z') &&
          (c(y + 2)(x + 16) == '#' || c(y+2)(x+16) == 'Z') &&
          (c(y + 1)(x + 17) == '#' || c(y+1)(x+17) == 'Z') &&
          (c(y + 0)(x + 18) == '#' || c(y)(x+18) == 'Z') &&
          (c(y + 1)(x + 18) == '#' || c(y+1)(x+18) == 'Z') &&
          (c(y + 1)(x + 19) == '#' || c(y+1)(x+19) == 'Z')
        ) {
          c(y+1)(x) = 'Z'
          c(y+2)(x+1) = 'Z'
          c(y+2)(x+4) = 'Z'
          c(y+1)(x+5) = 'Z'
          c(y+1)(x+6) = 'Z'
          c(y+2)(x+7) = 'Z'
          c(y+2)(x+10) = 'Z'
          c(y+1)(x+11) = 'Z'
          c(y+1)(x+12) = 'Z'
          c(y+2)(x+13) = 'Z'
          c(y+2)(x+16) = 'Z'
          c(y+1)(x+17) = 'Z'
          c(y)(x+18) = 'Z'
          c(y+1)(x+18) = 'Z'
          c(y+1)(x+19) = 'Z'
        }
      }
    }

    println(monster.map(_.mkString("")).mkString("\n"))


    val result =
      """
    2801 2203 3697 1249 1031 1867 1303 1723 2411 1871 3727 3823
    1319 1093 2281 3793 2797 2089 1163 1019 1979 1447 1657 3607
    1471 1367 2909 2081 3461 3889 3499 2539 3709 3373 3947 3169
    2713 1847 1933 2777 1427 3119 2749 2347 3821 1279 1697 2939
    3037 1747 3203 1051 1907 2473 2399 3659 1283 2591 1823 1889
    1307 1667 1187 1831 1583 1301 2237 2699 3041 2549 2887 1949
    3259 2063 3533 3079 1531 3307 2819 3191 2789 1153 3919 1087
    1063 2269 2711 3797 1559 1861 1109 1901 3917 2437 2791 2381
    1721 3347 3371 1973 2129 2803 2459 1523 1783 1801 1553 3881
    2917 1481 3833 1061 1549 1423 3049 3557 1913 2729 1787 1609
    1069 2003 2671 2879 3467 3449 3251 1607 1459 1811 1409 3271
    1759 2287 2663 2609 3769 3637 2011 2273 1699 3593 3529 2719
    """
    c.map(_.count(x => x == '#')).sum.toString
  }
}

object Day20 {
  def apply() = new Day20()

  case class Tile(id: Long, borderTop: String, borderBottom: String, borderLeft: String, borderRight: String)(val contents: Array[Array[Char]]) {
    def rotate: Tile = {
      val rotatedContents = Array.ofDim[Char](contents.length, contents(0).length)
      contents.indices.foreach { y =>
        contents(y).indices.foreach { x =>
          rotatedContents(x)(contents.length - 1 - y) = contents(y)(x)
        }
      }
      Tile(id, borderLeft.reverse, borderRight.reverse, borderBottom, borderTop)(rotatedContents)
    }

    def flipH: Tile = {
      Tile(id, borderTop.reverse, borderBottom.reverse, borderRight, borderLeft)(contents.map(_.reverse))
    }

    def flipV: Tile = {
      Tile(id, borderBottom, borderTop, borderLeft.reverse, borderRight.reverse)(contents.reverse)
    }
  }

  def fill(grid: Array[Array[Option[Tile]]], tiles: Map[Long, Tile]): Unit = {
    //    println(grid.map(r => r.map { c => c.map(_.id).getOrElse(0) }.mkString(" ")).mkString("\n"))
    findMostConstrained(grid) match {
      case None => {
        val tl = grid(0)(0).get.id
        val bl = grid(grid.length - 1)(0).get.id
        val tr = grid(0)(grid(0).length - 1).get.id
        val br = grid(grid.length - 1)(grid(grid.length - 1).length - 1).get.id
        println("Grid filled: tiles in corner are: " + tl + ", " + tr + ", " + bl + ", " + br)
        println("Product is: " + (tl * tr * bl * br))
      }
      case Some((y, x)) => {
        val candidates = generateCandidates(grid, (y, x), tiles)
        candidates.foreach { tile =>
          grid(y)(x) = Some(tile)
          fill(grid, tiles - tile.id)
          // To backtrack we need to clear the grid...but then how do we get the result?
        }
      }
    }
  }

  def findMostConstrained(grid: Array[Array[Option[Tile]]]): Option[(Int, Int)] = {
    val unfilled = grid.map(r => r.count(_.isEmpty)).sum
    if (unfilled == 0) {
      None
    } else if (unfilled == grid.length * grid(0).length) {
      Some((grid.length / 2, grid(0).length / 2))
    } else {
      var position: Option[(Int, Int)] = None
      var neighbours = 0
      grid.indices.foreach { y =>
        grid(y).indices.foreach { x =>
          val myNeighbours = (if (y > 0) grid(y - 1)(x).map(_ => 1).getOrElse(0) else 0) +
            (if (y < grid.length - 1) grid(y + 1)(x).map(_ => 1).getOrElse(0) else 0) +
            (if (x > 0) grid(y)(x - 1).map(_ => 1).getOrElse(0) else 0) +
            (if (x < grid(y).length - 1) grid(y)(x + 1).map(_ => 1).getOrElse(0) else 0)

          if (grid(y)(x).isEmpty && myNeighbours > neighbours) {
            neighbours = myNeighbours
            position = Some(y, x)
          }
        }
      }
      position
    }
  }

  def generateCandidates(grid: Array[Array[Option[Tile]]], position: (Int, Int), options: Map[Long, Tile]): Iterable[Tile] = {
    val topNeighbour = if (position._1 > 0) {
      grid(position._1 - 1)(position._2)
    } else None
    val bottomNeighbour = if (position._1 < grid.length - 1) {
      grid(position._1 + 1)(position._2)
    } else None
    val leftNeighbour = if (position._2 > 0) {
      grid(position._1)(position._2 - 1)
    } else None
    val rightNeighbour = if (position._2 < grid(position._1).length - 1) {
      grid(position._1)(position._2 + 1)
    } else None
    options.values.flatMap { x =>
      val orientations = allOrientations(x)
      val opts = (options - x.id).flatMap(t => allOrientations(t._2))
      orientations.filter { o =>
        val allowsTop = position._1 == 0 || (if (topNeighbour.isDefined) {
          topNeighbour.get.borderBottom == o.borderTop
        } else opts.exists(k => k.borderBottom == o.borderTop))
        val allowsLeft = position._2 == 0 || (if (leftNeighbour.isDefined) {
          leftNeighbour.get.borderRight == o.borderLeft
        } else opts.exists(k => k.borderRight == o.borderLeft))
        allowsTop && allowsLeft &&
          (rightNeighbour.isEmpty || rightNeighbour.get.borderLeft == o.borderRight) &&
          (bottomNeighbour.isEmpty || bottomNeighbour.get.borderTop == o.borderBottom) &&
          (position._1 == grid.length - 1 || bottomNeighbour.isDefined || opts.exists(k => k.borderTop == o.borderBottom)) &&
          (position._2 == grid(position._1).length - 1 || rightNeighbour.isDefined || opts.exists(k => k.borderLeft == o.borderRight))
      }
    }
  }

  private def allOrientations(x: Tile): Iterable[Tile] = {
    Seq(
      x,
      x.rotate,
      x.rotate.rotate,
      x.rotate.rotate.rotate,
      x.flipH,
      x.flipH.rotate,
      x.flipV,
      x.flipV.rotate,
    ).toSet
  }
}

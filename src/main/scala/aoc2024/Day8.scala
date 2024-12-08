package aoc2024

import aoc.Point

import scala.collection.mutable

class Day8 extends aoc.NewDay(2024, 8) {
  part(1) {
    test(
      """............
        |........0...
        |.....0......
        |.......0....
        |....0.......
        |......A.....
        |............
        |............
        |........A...
        |.........A..
        |............
        |............""".stripMargin -> 14)

    execute { ls =>
      val antennae = ls.flatMap(_.toCharArray.toSet).toSet - '.'
      val antennaPositions = mutable.Map[Char, Set[Point]]().withDefaultValue(Set())
      antennae.toList.foreach { freq =>
        ls.indices.foreach { c =>
          ls(c).indices.foreach { r =>
            if (ls(c)(r) == freq) {
              antennaPositions(freq) = antennaPositions(freq) + Point(c, r)
            }
          }
        }
      }
      val antinodes = mutable.Set[Point]()
      antennaPositions.foreach { case (_, pts) =>
        val ptsList = pts.toList
        ptsList.indices.dropRight(1).foreach { i =>
          ptsList.indices.drop(i + 1).foreach { j =>
            val dX = ptsList(j).x - ptsList(i).x
            val dY = ptsList(j).y - ptsList(i).y
            antinodes += Point(ptsList(i).x - dX, ptsList(i).y - dY)
            antinodes += Point(ptsList(j).x + dX, ptsList(j).y + dY)
          }
        }
      }
      antinodes.count { p => p.x >= 0 && p.y >= 0 && p.y < ls.length && p.x < ls(p.y).length }
    }
  }
  part(2) {
    test("""T.........
           |...T......
           |.T........
           |..........
           |..........
           |..........
           |..........
           |..........
           |..........
           |..........""".stripMargin -> 9)
    test(
      """............
        |........0...
        |.....0......
        |.......0....
        |....0.......
        |......A.....
        |............
        |............
        |........A...
        |.........A..
        |............
        |............""".stripMargin -> 34)

    execute { ls =>
      val antennae = ls.flatMap(_.toCharArray.toSet).toSet - '.'
      val antennaPositions = mutable.Map[Char, Set[Point]]().withDefaultValue(Set())
      antennae.toList.foreach { freq =>
        ls.indices.foreach { c =>
          ls(c).indices.foreach { r =>
            if (ls(c)(r) == freq) {
              antennaPositions(freq) = antennaPositions(freq) + Point(c, r)
            }
          }
        }
      }
      val antinodes = mutable.Set[Point]()
      antennaPositions.foreach { case (_, pts) =>
        val ptsList = pts.toList
        ptsList.indices.dropRight(1).foreach { i =>
          ptsList.indices.drop(i + 1).foreach { j =>
            val dX = ptsList(j).x - ptsList(i).x
            val dY = ptsList(j).y - ptsList(i).y
            var iX = ptsList(i).x
            var iY = ptsList(i).y
            while (iX >= 0 && iY >= 0 && iY < ls.length && iX < ls(iY).length) {
              antinodes += Point(iX, iY)
              iX = iX - dX
              iY = iY - dY
            }
            var jX = ptsList(j).x
            var jY = ptsList(j).y
            while (jX >= 0 && jY >= 0 && jY < ls.length && jX < ls(jY).length) {
              antinodes += Point(jX, jY)
              jX = jX + dX
              jY = jY + dY
            }
          }
        }
      }
      antinodes.count { p => p.x >= 0 && p.y >= 0 && p.y < ls.length && p.x < ls(p.y).length }
    }
  }
}

object Day8Main extends Day8

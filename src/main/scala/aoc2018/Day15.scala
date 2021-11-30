package aoc2018

import aoc.{Day, Point, Search}

import scala.util.control.Breaks.{break, breakable}

class Day15 extends Day(2018, 15) {

  import Day15._


  val example1 =
    """#######
      |#E..G.#
      |#...#.#
      |#.G.#G#
      |#######""".stripMargin.split("\n")

  val example2 =
    """#########
      |#G..G..G#
      |#.......#
      |#.......#
      |#G..E..G#
      |#.......#
      |#.......#
      |#G..G..G#
      |#########""".stripMargin.split("\n")

  val example3 =
    """#######
      |#.G...#
      |#...EG#
      |#.#.#G#
      |#..G#E#
      |#.....#
      |#######""".stripMargin.split("\n")

  val example4 =
    """#######
      |#G..#E#
      |#E#E.E#
      |#G.##.#
      |#...#E#
      |#...E.#
      |#######""".stripMargin.split("\n")

  val example5 = """#######
                   |#E..EG#
                   |#.#G.E#
                   |#E.##E#
                   |#G..#.#
                   |#..E#.#
                   |#######""".stripMargin.split("\n")

  val example6 = """#######
                   |#E.G#.#
                   |#.#G..#
                   |#G.#.G#
                   |#G..#.#
                   |#...E.#
                   |#######""".stripMargin.split("\n")

  val example7 = """#######
                   |#.E...#
                   |#.#..G#
                   |#.###.#
                   |#E#G#G#
                   |#...#G#
                   |#######""".stripMargin.split("\n")

  val example8 = """#########
                   |#G......#
                   |#.E.#...#
                   |#..##..G#
                   |#...##..#
                   |#...#...#
                   |#.G...G.#
                   |#.....G.#
                   |#########""".stripMargin.split("\n")

  override def part1: String = {
    val grid: Array[Array[Char]] = input.map(_.toCharArray)
    val unitsB = Seq.newBuilder[Unit]
    var idx = 0
    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        if (grid(y)(x) == 'G') {
          unitsB.addOne(new Goblin(idx, Point(x, y)))
          idx = idx + 1
        } else if (grid(y)(x) == 'E') {
          unitsB.addOne(new Elf(idx, Point(x, y)))
          idx = idx + 1
        }
      }
    }
    var units = unitsB.result()
    var rounds = 0
    breakable {
      while (true) {
        if (debug) {
          println(rounds)
          printGrid(grid)
        }
        units.sortBy(u => (u.p.y, u.p.x)).foreach { u =>
          if (u.hp > 0) {
            val targets = if (u.isInstanceOf[Goblin]) {
              units.filter(_.isInstanceOf[Elf])
            } else {
              units.filter(_.isInstanceOf[Goblin])
            }
            if (targets.isEmpty) {
              break
            } else {
              if (!targets.exists(t => t.p.manhattanDistanceTo(u.p) == 1)) {
                val targetSpaces = targets.flatMap(u => u.p.neighbours).filter(pt => grid(pt.y)(pt.x) == '.')
                val reachable = targetSpaces.map { tp =>
                  (tp, Search.AStar[Point](u.p, tp, pt => pt.neighbours.filter(p_ => grid(p_.y)(p_.x) == '.'), pt => pt.manhattanDistanceTo(tp)))
                }.filter(_._2.nonEmpty)
                if (reachable.nonEmpty) {
                  val nearest = reachable.groupBy(_._2.length).minBy(_._1)._2
                  val path = nearest.minBy(p => (p._1.y, p._1.x))._2
                  val step = path.drop(1).head
                  grid(u.p.y)(u.p.x) = '.'
                  u.p = step
                  grid(u.p.y)(u.p.x) = if (u.isInstanceOf[Elf]) 'E' else 'G'
                }
              }
              if (targets.exists(t => t.p.manhattanDistanceTo(u.p) == 1)) {
                val target = targets.filter(t => t.p.manhattanDistanceTo(u.p) == 1).minBy(t => (t.hp, t.p.y, t.p.x))
                target.hp = target.hp - u.ap
                if (target.hp <= 0) {
                  units = units.filter { u => u.hp > 0 }
                  grid(target.p.y)(target.p.x) = '.'
                }
              }
            }
          }
        }
        rounds = rounds + 1
      }
    }
    if (debug) {
      printGrid(grid)
    }
    (rounds * units.map(_.hp).sum).toString
  }

  def printGrid(array: Array[Array[Char]]) {
    println(array.map(_.mkString).mkString("\n"))
  }

  def printInRange(array: Array[Array[Char]], value: Seq[aoc.Point]) {
    array.indices.foreach { y =>
      array(y).indices.foreach { x =>
        if (value.exists(p => p.x == x && p.y == y)) {
          print('?')
        } else {
          print(array(y)(x))
        }
      }
      println()
    }
  }

  def printReachable(array: Array[Array[Char]], value: Seq[aoc.Point]) {
    array.indices.foreach { y =>
      array(y).indices.foreach { x =>
        if (value.exists(p => p.x == x && p.y == y)) {
          print('@')
        } else {
          print(array(y)(x))
        }
      }
      println()
    }
  }

  def printNearest(array: Array[Array[Char]], value: Seq[aoc.Point]) {
    array.indices.foreach { y =>
      array(y).indices.foreach { x =>
        if (value.exists(p => p.x == x && p.y == y)) {
          print('!')
        } else {
          print(array(y)(x))
        }
      }
      println()
    }
  }

  def printChosen(array: Array[Array[Char]], value: aoc.Point) {
    array.indices.foreach { y =>
      array(y).indices.foreach { x =>
        if (value.x == x && value.y == y) {
          print('+')
        } else {
          print(array(y)(x))
        }
      }
      println()
    }
  }

  override def part2: String = {
    val grid: Array[Array[Char]] = input.map(_.toCharArray)
    val unitsB = Seq.newBuilder[Unit]
    var idx = 0
    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        if (grid(y)(x) == 'G') {
          unitsB.addOne(new Goblin(idx, Point(x, y)))
          idx = idx + 1
        } else if (grid(y)(x) == 'E') {
          unitsB.addOne(new Elf(idx, Point(x, y), 19))
          idx = idx + 1
        }
      }
    }
    var units = unitsB.result()
    var rounds = 0
    breakable {
      while (true) {
        if (debug) {
          println(rounds)
          printGrid(grid)
        }
        units.sortBy(u => (u.p.y, u.p.x)).foreach { u =>
          if (u.hp > 0) {
            val targets = if (u.isInstanceOf[Goblin]) {
              units.filter(_.isInstanceOf[Elf])
            } else {
              units.filter(_.isInstanceOf[Goblin])
            }
            if (targets.isEmpty) {
              break
            } else {
              if (!targets.exists(t => t.p.manhattanDistanceTo(u.p) == 1)) {
                val targetSpaces = targets.flatMap(u => u.p.neighbours).filter(pt => grid(pt.y)(pt.x) == '.')
                val reachable = targetSpaces.map { tp =>
                  (tp, Search.AStar[Point](u.p, tp, pt => pt.neighbours.filter(p_ => grid(p_.y)(p_.x) == '.'), pt => pt.manhattanDistanceTo(tp)))
                }.filter(_._2.nonEmpty)
                if (reachable.nonEmpty) {
                  val nearest = reachable.groupBy(_._2.length).minBy(_._1)._2
                  val path = nearest.minBy(p => (p._1.y, p._1.x))._2
                  val step = path.drop(1).head
                  grid(u.p.y)(u.p.x) = '.'
                  u.p = step
                  grid(u.p.y)(u.p.x) = if (u.isInstanceOf[Elf]) 'E' else 'G'
                }
              }
              if (targets.exists(t => t.p.manhattanDistanceTo(u.p) == 1)) {
                val target = targets.filter(t => t.p.manhattanDistanceTo(u.p) == 1).minBy(t => (t.hp, t.p.y, t.p.x))
                target.hp = target.hp - u.ap
                if (target.hp <= 0) {
                  units = units.filter { u => u.hp > 0 }
                  grid(target.p.y)(target.p.x) = '.'
                }
              }
            }
          }
        }
        rounds = rounds + 1
      }
    }
    if (debug) {
      printGrid(grid)
    }
    (rounds * units.map(_.hp).sum).toString
  }
}

object Day15 {
  def apply() = new Day15()

  abstract class Unit(val idx: Int, val ap: Int) {
    var p: Point = _
    var hp: Int = _

    override def toString: String = this.getClass.getName + "(" + idx + ", " + p + ", " + ap + ", " + hp + ")"
  }

  class Goblin(id: Int, pt: Point) extends Unit(id, 3) {
    hp = 200
    p = pt
  }

  class Elf(id: Int, pt: Point, ap: Int = 3) extends Unit(id, ap) {
    hp = 200
    p = pt
  }

}

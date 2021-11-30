package aoc2018

import aoc.{Day, Direction, Point}
import aoc.Direction.Direction

import scala.collection.mutable

class Day13 extends Day(2018, 13) {

  import Day13._

  def parseInput(input: Array[String]): (Array[Array[Char]], List[Cart]) = {
    val grid = input.map(_.toCharArray)
    val carts = mutable.Buffer[Cart]()
    var cartIdx = 1
    grid.indices.foreach { y =>
      grid(y).indices.foreach { x =>
        grid(y)(x) match {
          case '^' => carts.addOne(Cart(cartIdx, Point(x, y), Direction.North, 0))
            cartIdx = cartIdx + 1
          case '>' => carts.addOne(Cart(cartIdx, Point(x, y), Direction.East, 0))
            cartIdx = cartIdx + 1
          case '<' => carts.addOne(Cart(cartIdx, Point(x, y), Direction.West, 0))
            cartIdx = cartIdx + 1
          case 'v' => carts.addOne(Cart(cartIdx, Point(x, y), Direction.South, 0))
            cartIdx = cartIdx + 1
          case _ =>
        }
      }
    }
    (grid, carts.toList)
  }

  def moveCart(c: Cart, grid: Array[Array[Char]]): Cart = {
    var nextTurn = c.turn
    val nP = c.d match {
      case Direction.North => c.p.copy(y = c.p.y - 1)
      case Direction.South => c.p.copy(y = c.p.y + 1)
      case Direction.East => c.p.copy(x = c.p.x + 1)
      case Direction.West => c.p.copy(x = c.p.x - 1)
    }
    val nD = grid(nP.y)(nP.x) match {
      case '/' => c.d match {
        case Direction.North => Direction.East
        case Direction.East => Direction.North
        case Direction.South => Direction.West
        case Direction.West => Direction.South
      }
      case '\\' => c.d match {
        case Direction.North => Direction.West
        case Direction.East => Direction.South
        case Direction.South => Direction.East
        case Direction.West => Direction.North
      }
      case '+' =>
        nextTurn = (c.turn + 1) % 3
        (c.d, c.turn) match {
          case (Direction.North, 0) => Direction.West
          case (Direction.North, 1) => Direction.North
          case (Direction.North, 2) => Direction.East
          case (Direction.East, 0) => Direction.North
          case (Direction.East, 1) => Direction.East
          case (Direction.East, 2) => Direction.South
          case (Direction.South, 0) => Direction.East
          case (Direction.South, 1) => Direction.South
          case (Direction.South, 2) => Direction.West
          case (Direction.West, 0) => Direction.South
          case (Direction.West, 1) => Direction.West
          case (Direction.West, 2) => Direction.North
        }
      case _ => c.d
    }
    Cart(c.idx, nP, nD, nextTurn)
  }

  override def part1: String = {
    var (grid, carts) = parseInput(input)
    while (!carts.exists(c => carts.exists(c2 => c2.p == c.p && c.idx != c2.idx))) {
      carts = carts.sortBy(f => (f.p.y, f.p.x)).map { c => moveCart(c, grid) }
    }
    carts.find(c => carts.exists(c2 => c2.p == c.p && c.idx != c2.idx)).get.p.toString
  }

  val example = """/>-<\
                  ||   |
                  || /<+-\
                  || | | v
                  |\>+</ |
                  |  |   ^
                  |  \<->/""".stripMargin.split("\n")

  override def part2: String = {
    var (grid, carts) = parseInput(input)
    while (carts.size > 1) {
      val crashes = Set.newBuilder[Int]
      val nCarts = mutable.Buffer[Cart]()
      carts = carts.sortBy(f => (f.p.y, f.p.x))
      while (carts.nonEmpty) {
        val c = carts.head
        carts = carts.tail
        val nc = moveCart(c, grid)
        if (carts.exists(c2 => c2.p == nc.p && nc.idx != c2.idx)) {
          val c2 = carts.find(c2 => c2.p == nc.p && nc.idx != c2.idx)
          crashes.addOne(nc.idx)
          crashes.addOne(c2.get.idx)
        }
        if (nCarts.exists(c2 => c2.p == nc.p && nc.idx != c2.idx)) {
          val c2 = nCarts.find(c2 => c2.p == nc.p && nc.idx != c2.idx)
          crashes.addOne(nc.idx)
          crashes.addOne(c2.get.idx)
        }
        nCarts.addOne(nc)
      }
      val crashedCarts = crashes.result()
      carts = nCarts.filter{c => !crashedCarts.contains(c.idx)}.toList
    }
    carts.head.p.toString
  }
}

object Day13 {
  def apply() = new Day13()

  case class Cart(idx: Int, p: Point, d: Direction, turn: Int)

}

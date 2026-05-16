package aoc2016

import aoc.{NewDay, Search}

import java.security.MessageDigest
import scala.collection.mutable

class Day17 extends NewDay(2016, 17) {
  import Day17._

  part(1) {
    execute { in =>
      val hash = in(0)
      Search.breadthFirst[Square](Square(0, 0, hash)(None), neighbours, s => s.x == 3 && s.y == 3).path.drop(hash.length)
    }
  }

  def neighbours(s: Square): Set[Square] = s.neighbours

  part(2) {
    execute { in =>
      val hash = in(0)
      val result = findAllPaths[Square](Square(0, 0, hash)(None), neighbours, s => s.x == 3 && s.y == 3)
      val out = result.maxBy(s => s.path.length)
      (out.path.length - hash.length).toString
    }
  }

  def findAllPaths[A](root: A, f: A => Set[A], isGoal: A => Boolean): Set[A] = {
    val successfulPaths = Set.newBuilder[A]
    val seen = mutable.Set[A](root)
    val q = mutable.Queue(root)

    while (q.nonEmpty) {
      val v = q.dequeue()
      if (isGoal(v)) {
        successfulPaths.addOne(v)
      } else {
        val ws = f(v)
        ws.foreach { w =>
          if (!seen.contains(w)) {
            seen.addOne(w)
            q.addOne(w)
          }
        }
      }
    }
    successfulPaths.result()
  }
}

object Day17 {
  case class Square(x: Int, y: Int, path: String)(val from: Option[Square]) {
    val md: MessageDigest = MessageDigest.getInstance("MD5")
    def neighbours: Set[Square] = {
      val d = md.digest(path.getBytes).take(2).map("%02x" format _).mkString("")
      val n = Set.newBuilder[Square]
      if (y>0 && d(0) >= 'b' && d(0) <= 'f') {
        n.addOne(Square(x, y-1, path + "U")(Some(this)))
      }
      if (y < 3 && d(1) >= 'b' && d(1) <= 'f') {
        n.addOne(Square(x, y+1, path + "D")(Some(this)))
      }
      if (x>0 && d(2) >= 'b' && d(2) <= 'f') {
        n.addOne(Square(x-1, y, path + "L")(Some(this)))
      }
      if (x<3 && d(3) >= 'b' && d(3) <= 'f') {
        n.addOne(Square(x+1, y, path + "R")(Some(this)))
      }
      n.result()
    }
  }
}

object Day17Main extends Day17

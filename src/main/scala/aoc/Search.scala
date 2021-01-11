package aoc

import scala.collection.mutable

object Search {

  def breadthFirst[A](root: A, transitionFunc: A => Set[A], isGoal: A => Boolean): A = {
    val seen = mutable.Set[A](root)
    val q = mutable.Queue(root)

    while (q.nonEmpty) {
      val v = q.dequeue()
      if (isGoal(v)) {
        return v
      }
      val ws = transitionFunc(v)
      ws.foreach { w =>
        if (!seen.contains(w)) {
          seen.addOne(w)
          q.addOne(w)
        }
      }
    }
    root
  }

  def AStar[A](start: A, goal: A, neighbours: A => Iterable[A], h: A => Int): List[A] = {
    def reconstructPath(cameFrom: mutable.Map[A, A], a: A): List[A] = {
      val o = mutable.Buffer[A](a)
      var current = a
      while (cameFrom.contains(current)) {
        current = cameFrom(current)
        o.prepend(current)
      }
      o.toList
    }

    val cameFrom = mutable.Map[A, A]()
    val gScore = mutable.Map[A, Int](start -> 0)
    val fScore = mutable.Map[A, Int](start -> h(start))

    val openSet = mutable.Queue[A](start)
    while (openSet.nonEmpty) {
      val current = openSet.dequeue()
      if (current == goal) {
        return reconstructPath(cameFrom, current)
      }

      neighbours(current).foreach { neighbour =>
        val tentativeGScore = gScore.getOrElse(current, Int.MaxValue) + 1
        if (tentativeGScore < gScore.getOrElse(neighbour, Int.MaxValue)) {
          cameFrom(neighbour) = current
          gScore(neighbour) = tentativeGScore
          fScore(neighbour) = tentativeGScore + h(neighbour)
          if (!openSet.contains(neighbour)) {
            openSet.addOne(neighbour)
          }
        }
      }
    }
    List()
  }
}

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
}

package aoc

class DisjointSet {
  def makeSet(t: Elem): Unit = {
    t.parent = Some(t)
    t.size = 1
  }

  def find(t1: Elem): Elem = {
    var root = t1
    while (root.parent.get != root) {
      root = root.parent.get
    }
    var x = t1
    while (x.parent.get != root) {
      val parent = x.parent.get
      x.parent = Some(root)
      x = parent
    }
    root
  }

  def union(t1: Elem, t2: Elem): Unit = {
    var x = find(t1)
    var y = find(t2)
    if (x == y) {
      return
    }
    if (x.size < y.size) {
      val temp = x
      x = y
      y = temp
    }
    y.parent = Some(x)
    x.size = x.size + y.size
  }
}

class Elem {
  var parent: Option[Elem] = None
  var size: Int = 0
}
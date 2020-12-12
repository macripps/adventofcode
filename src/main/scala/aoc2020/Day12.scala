package aoc2020

object Day12 {

  def main(): Unit = {
    val lines = readFileToIterable("aoc2020/day12.input")

    part1(lines)
    part2(lines)
  }

  def part1(lines: Iterable[String]) = {
    var pos = (0, 0)
    val dirs = Seq((1, 0), (0, -1), (-1, 0), (0, 1))
    var dir = 0
    lines.foreach { l =>
      val direction = l.charAt(0)
      var amount = l.drop(1).toInt
      val delta = direction match {
        case 'F' => (dirs(dir)._1 * amount, dirs(dir)._2 * amount)
        case 'N' => (0, amount)
        case 'E' => (amount, 0)
        case 'S' => (0, -amount)
        case 'W' => (-amount, 0)
        case 'L' => {
          while (amount > 0) {
            dir = dir - 1
            amount = amount - 90
          }
          if (dir < 0) dir = dir + dirs.length
          (0, 0)
        }
        case 'R' => {
          while (amount > 0) {
            dir = (dir + 1) % dirs.length
            amount = amount - 90
          }
          (0, 0)
        }
      }
      pos = (pos._1 + delta._1, pos._2 + delta._2)
    }

    println("Manhattan Distance: " + (math.abs(pos._1) + math.abs(pos._2)))
  }

  def part2(lines: Iterable[String]): Unit = {
    var pos = (0, 0)
    var waypoint = (10, 1)
    lines.foreach { l =>
      val direction = l.charAt(0)
      var amount = l.drop(1).toInt
      direction match {
        case 'F' =>
          pos = (pos._1 + waypoint._1 * amount, pos._2 + waypoint._2 * amount)
        case 'N' =>
          waypoint = (waypoint._1, waypoint._2 + amount)
        case 'E' =>
          waypoint = (waypoint._1 + amount, waypoint._2)
        case 'S' =>
          waypoint = (waypoint._1, waypoint._2 - amount)
        case 'W' =>
          waypoint = (waypoint._1 - amount, waypoint._2)
        case 'L' =>
          while (amount > 0) {
            waypoint = (-waypoint._2, waypoint._1)
            amount = amount - 90
          }
        case 'R' =>
          while (amount > 0) {
            waypoint = (waypoint._2, -waypoint._1)
            amount = amount - 90
          }
      }
    }
    println("Manhattan Distance: " + (math.abs(pos._1) + math.abs(pos._2)))
  }
}

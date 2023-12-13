package aoc2023

import aoc.Strings

class Day13 extends aoc.NewDay(2023, 13) {

  part(1) {
    test(
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.
        |
        |#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#""".stripMargin -> 405)

    execute { input =>
      val patterns = aoc.asGroupsSeparatedByBlankLines(input)
      patterns.map { pattern =>
         // Look for horizontal
        val pA = pattern.toArray
        val horizVal = findLine(pA).getOrElse(0)
        val pB = pattern.map(_.toCharArray).transpose.map(c => new String(c.toArray)).toArray
        val vertVal = findLine(pB).getOrElse(0)
        if (horizVal == 0 && vertVal == 0) {
          println("Unable to find reflection in:\n" + pattern.mkString("\n"))
        }
        (horizVal * 100) + vertVal
      }.sum
    }
  }

  part(2) {
    test("""#.##..##.
           |..#.##.#.
           |##......#
           |##......#
           |..#.##.#.
           |..##..##.
           |#.#.##.#.
           |
           |#...##..#
           |#....#..#
           |..##..###
           |#####.##.
           |#####.##.
           |..##..###
           |#....#..#""".stripMargin -> 400)

    execute { input =>
      val patterns = aoc.asGroupsSeparatedByBlankLines(input)
      patterns.map { pattern =>
        // Look for horizontal
        val pA = pattern.toArray
        val horizVal = findLineWithSmudge(pA).getOrElse(0)
        val pB = pattern.map(_.toCharArray).transpose.map(c => new String(c.toArray)).toArray
        val vertVal = findLineWithSmudge(pB).getOrElse(0)
        if (horizVal == 0 && vertVal == 0) {
          println("Unable to find reflection in:\n" + pattern.mkString("\n"))
        }
        (horizVal * 100) + vertVal
      }.sum
    }
  }

  private[this] def findLine(grid: Array[String]): Option[Int] = {
    (1 until grid.length).find { lineAfter =>
      var checkBefore = lineAfter - 1
      var checkAfter = lineAfter
      var found = true
      while (found && checkBefore >= 0 && checkAfter < grid.length) {
        found = found && grid(checkBefore) == grid(checkAfter)
        checkBefore = checkBefore - 1
        checkAfter = checkAfter + 1
      }
      found
    }
  }

  private[this] def findLineWithSmudge(grid: Array[String]): Option[Int] = {
    (1 until grid.length).find { lineAfter =>
      var checkBefore = lineAfter - 1
      var checkAfter = lineAfter
      var found = true
      var madeSmudge = false
      while (found && checkBefore >= 0 && checkAfter < grid.length) {
        var smudged = false
        val gcb = grid(checkBefore)
        val gca = grid(checkAfter)
        if (!madeSmudge && Strings.levenshteinDistance(gcb, gca) == 1) {
          madeSmudge = true
          smudged = true
        }
        found = found && (gca == gcb || (smudged))
        checkBefore = checkBefore - 1
        checkAfter = checkAfter + 1
      }
      madeSmudge && found
    }
  }

}

object Day13Main extends Day13

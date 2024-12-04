package aoc2024

class Day4 extends aoc.NewDay(2024, 4) {
  part(1) {
    test(
      """MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX""".stripMargin -> 18)

    execute { ls =>
      val word = "XMAS".toCharArray

      ls.indices.flatMap { col =>
        ls(col).indices.map { row =>
          if (ls(col)(row) == 'X') {
            Array(
              (-1, -1),
              (-1, 0),
              (-1, 1),
              (0, -1),
              (0, 1),
              (1, -1),
              (1, 0),
              (1, 1)
            ).count { diff =>
              col + diff._2 * 3 >= 0  &&
              col + diff._2 * 3 < ls.length &&
              row + diff._1 * 3 >= 0 &&
              row + diff._1 * 3 < ls(col).length &&
              ls(col + diff._2)(row + diff._1) == 'M' &&
              ls(col + diff._2 + diff._2)(row + diff._1 + diff._1) == 'A' &&
              ls(col + diff._2 + diff._2 + diff._2)(row + diff._1 + diff._1 + diff._1) == 'S'
            }
          } else 0
        }
      }.sum
    }

    part(2) {
      test(
        """MMMSXXMASM
          |MSAMXMSMSA
          |AMXSXMAAMM
          |MSAMASMSMX
          |XMASAMXAMM
          |XXAMMXXAMA
          |SMSMSASXSS
          |SAXAMASAAA
          |MAMMMXMMMM
          |MXMXAXMASX""".stripMargin -> 9)

      execute { ls =>
        ls.indices.drop(1).dropRight(1).map { col =>
          ls(col).indices.drop(1).dropRight(1).count { row =>
            (ls(col)(row) == 'A' &&
              ((ls(col-1)(row-1) == 'M' && ls(col+1)(row+1) == 'S')
                ||
                (ls(col+1)(row+1) == 'M' && ls(col-1)(row-1) == 'S'))
            && ((ls(col-1)(row+1) == 'M' && ls(col+1)(row-1) == 'S')
              ||
              (ls(col+1)(row-1) == 'M' && ls(col-1)(row+1) == 'S')))
          }
        }.sum
      }
    }
  }
}

object Day4Main extends Day4

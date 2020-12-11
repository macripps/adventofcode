package aoc2020

import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  test("iterations of example") {
    val ex1 =
      """L.LL.LL.LL
        |LLLLLLL.LL
        |L.L.L..L..
        |LLLL.LL.LL
        |L.LL.LL.LL
        |L.LLLLL.LL
        |..L.L.....
        |LLLLLLLLLL
        |L.LLLLLL.L
        |L.LLLLL.LL""".stripMargin.split("\n").map(_.toCharArray)

    println(ex1.map(_.mkString("")).mkString("\n"))
    val out = Day11.iterate1(ex1)
    val expected =
      """#.##.##.##
        |#######.##
        |#.#.#..#..
        |####.##.##
        |#.##.##.##
        |#.#####.##
        |..#.#.....
        |##########
        |#.######.#
        |#.#####.##""".stripMargin.split("\n").map(_.toCharArray)

    assert(!Day11.different(out, expected))

    val out2 = Day11.iterate1(out)
    val expected2 =
      """#.LL.L#.##
        |#LLLLLL.L#
        |L.L.L..L..
        |#LLL.LL.L#
        |#.LL.LL.LL
        |#.LLLL#.##
        |..L.L.....
        |#LLLLLLLL#
        |#.LLLLLL.L
        |#.#LLLL.##""".stripMargin.split("\n").map(_.toCharArray)

    assert(!Day11.different(out2, expected2))
  }

  test("adjacency") {
    val grid =
      """#.LL.L#.##
        |#LLLLLL.L#
        |L.L.L..L..
        |#LLL.LL.L#
        |#.LL.LL.LL
        |#.LLLL#.##
        |..L.L.....
        |#LLLLLLLL#
        |#.LLLLLL.L
        |#.#LLLL.##""".stripMargin.split("\n").map(_.toCharArray)

    assert(Day11.adjacency1(grid, 0, 0) === 1)
    assert(Day11.adjacency1(grid, 1, 1) === 2)
    assert(Day11.adjacency1(grid, 2, 2) === 0)
    assert(Day11.adjacency1(grid, 0, 9) === 2)
  }

  test("visibility part 2") {
    val grid =
      """.......#.
        |...#.....
        |.#.......
        |.........
        |..#L....#
        |....#....
        |.........
        |#........
        |...#.....""".stripMargin.split("\n").map(_.toCharArray)

    assert(Day11.adjacency2(grid, 4, 3) === 8)

    val grid2 =
      """.............
        |.L.L.#.#.#.#.
        |.............""".stripMargin.split("\n").map(_.toCharArray)

    assert(Day11.adjacency2(grid2, 1, 1) === 0)

  }
}

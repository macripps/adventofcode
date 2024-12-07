package aoc2024

class Day7 extends aoc.NewDay(2024, 7) {
  part(1) {
    test("""190: 10 19
           |3267: 81 40 27
           |83: 17 5
           |156: 15 6
           |7290: 6 8 6 15
           |161011: 16 10 13
           |192: 17 8 14
           |21037: 9 7 18 13
           |292: 11 6 16 20""".stripMargin -> 3749)

    execute { ls =>
      ls.map { l =>
        val Array(test, nums) = l.split(": ")
        (test.toLong, nums.split(" ").map(_.toLong).toList)
      }.filter { case (test, nums) =>
        anyPossibilities(nums, test).contains(test)
      }.map { case (test, _) =>
        test
      }.sum
    }

    def anyPossibilities(ints: List[Long], max: Long): Stream[Long] = {
      if (ints.isEmpty) {
        Stream()
      } else if (ints.length == 1) {
        Stream(ints.head)
      } else {
        val l = ints.head
        if (l > max) {
          Stream()
        } else {
          val r = ints.drop(1).head
          val rest = ints.drop(2)
          anyPossibilities((l * r) :: rest, max) #::: anyPossibilities((l + r) :: rest, max)
        }
      }
    }
  }
  part(2) {
    test(
      """190: 10 19
        |3267: 81 40 27
        |83: 17 5
        |156: 15 6
        |7290: 6 8 6 15
        |161011: 16 10 13
        |192: 17 8 14
        |21037: 9 7 18 13
        |292: 11 6 16 20""".stripMargin -> 11387)

    execute { ls =>
      ls.map { l =>
        val Array(test, nums) = l.split(": ")
        (test.toLong, nums.split(" ").map(_.toLong).toList)
      }.filter { case (test, nums) =>
        anyPossibilities(nums, test).contains(test)
      }.map { case (test, _) =>
        test
      }.sum
    }

    def anyPossibilities(ints: List[Long], max: Long): Stream[Long] = {
      if (ints.isEmpty) {
        Stream()
      } else if (ints.length == 1) {
        Stream(ints.head)
      } else {
        val l = ints.head
        if (l > max) {
          Stream()
        } else {
          val r = ints.drop(1).head
          val rest = ints.drop(2)
          anyPossibilities((l.toString + r.toString).toLong :: rest, max) #::: anyPossibilities((l * r) :: rest, max) #::: anyPossibilities((l + r) :: rest, max)
        }
      }
    }
  }
}

object Day7Main extends Day7

package aoc2024

class Day5 extends aoc.NewDay(2024, 5) {
  part(1) {
    test(
      """47|53
        |97|13
        |97|61
        |97|47
        |75|29
        |61|13
        |75|53
        |29|13
        |97|29
        |53|29
        |61|53
        |97|53
        |61|29
        |47|13
        |75|47
        |97|75
        |47|61
        |75|61
        |47|29
        |75|13
        |53|13
        |
        |75,47,61,53,29
        |97,61,53,29,13
        |75,29,13
        |75,97,47,61,53
        |61,13,29
        |97,13,75,29,47""".stripMargin -> 143)

    execute { ls =>
      val data = aoc.asGroupsSeparatedByBlankLines(ls)
      val inputs = data.head.toSet
      val updates = data.tail.head.toList

      val x = new Ordering[String] {
        override def compare(x: String, y: String): Int = if (inputs.contains(s"${x}|${y}")) -1 else if (inputs.contains(s"${y}|${x}")) 1 else 0
      }

      updates.map { update =>
        val pages = update.split(",")
        if (pages.sorted(x) sameElements pages) {
          pages(pages.length / 2).toInt
        } else 0
      }.sum
    }
  }

  part(2) {
    test(
      """47|53
        |97|13
        |97|61
        |97|47
        |75|29
        |61|13
        |75|53
        |29|13
        |97|29
        |53|29
        |61|53
        |97|53
        |61|29
        |47|13
        |75|47
        |97|75
        |47|61
        |75|61
        |47|29
        |75|13
        |53|13
        |
        |75,47,61,53,29
        |97,61,53,29,13
        |75,29,13
        |75,97,47,61,53
        |61,13,29
        |97,13,75,29,47""".stripMargin -> 123)

    execute { ls =>
      val data = aoc.asGroupsSeparatedByBlankLines(ls)
      val inputs = data.head.toSet
      val updates = data.tail.head.toList

      val x = new Ordering[String] {
        override def compare(x: String, y: String): Int = if (inputs.contains(s"${x}|${y}")) -1 else if (inputs.contains(s"${y}|${x}")) 1 else 0
      }

      updates.map { update =>
        val pages = update.split(",")
        val sorted = pages.sorted(x)
        if (pages.sameElements(sorted)) {
          0
        } else sorted(sorted.length / 2).toInt
      }.sum
    }
  }
}

object Day5Main extends Day5

package aoc2023

import aoc.NewDay
import Math.max

class Day2 extends NewDay(2023, 2) {
  import Day2._

  part(1) {
    test(
      """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin -> 8)

    execute { in =>
      val maxRed = 12
      val maxGreen = 13
      val maxBlue = 14
      in.map { line =>
        val gameId = line.drop(5).takeWhile(c => c.isDigit).toInt
        val gameText = line.dropWhile(c => c != ':').drop(2)
        val rounds = gameText.split("; ")
        val roundsOk = rounds.forall { round =>

          val blues = blueR.findFirstMatchIn(round)
          val blueOk = blues.isEmpty || blues.get.group(1).toInt <= maxBlue
          val reds = redR.findFirstMatchIn(round)
          val redOk = reds.isEmpty || reds.get.group(1).toInt <= maxRed
          val greens = greenR.findFirstMatchIn(round)
          val greenOk = greens.isEmpty || greens.get.group(1).toInt <= maxGreen
          val roundOk = blueOk && redOk && greenOk
          roundOk
        }
        if (roundsOk) gameId else 0
      }.sum
    }
  }

  part(2) {
    test("""Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
           |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
           |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
           |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
           |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin -> 2286)

    execute { in =>

      in.map { line =>
        var minRed = 0
        var minGreen = 0
        var minBlue = 0
        val gameText = line.dropWhile(c => c != ':').drop(2)
        val rounds = gameText.split("; ")
        rounds.foreach { round =>
          val blues = blueR.findFirstMatchIn(round)
          blues.foreach { nBlues =>
            minBlue = max(minBlue, nBlues.group(1).toInt)
          }
          val reds = redR.findFirstMatchIn(round)
          reds.foreach { nReds =>
            minRed = max(minRed, nReds.group(1).toInt)
          }
          val greens = greenR.findFirstMatchIn(round)
          greens.foreach { nGreens =>
            minGreen = max(minGreen, nGreens.group(1).toInt)
          }
        }
        minGreen * minBlue * minRed
      }.sum
    }
  }
}

object Day2 {
  val blueR = "(\\d+) blue".r
  val redR = "(\\d+) red".r
  val greenR = "(\\d+) green".r
}

object Day2Main extends Day2

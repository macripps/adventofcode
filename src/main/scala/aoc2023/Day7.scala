package aoc2023

import scala.annotation.tailrec
import scala.collection.mutable

class Day7 extends aoc.Day(2023, 7) {

  import Day7._

  withPart1Test(
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin, 6440)

  override def part1(input: Array[String]): Any = {
    val handsWithBids = input.map { line =>
      val Array(hand, bid) = line.split(' ')
      HandWithBid(Hand(hand), bid.toLong)
    }

    handsWithBids.sorted.zipWithIndex.map { case (a, i) =>
      a.bid * (i.toLong + 1L)
    }.sum
  }

  withPart2Test(
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin, 5905)

  override def part2(input: Array[String]): Any = {
    val handsWithBids = input.map { line =>
      val Array(hand, bid) = line.split(' ')
      WildHandWithBid(WildHand(hand), bid.toLong)
    }

    handsWithBids.sorted.zipWithIndex.map { case (a, i) =>
      a.bid * (i.toLong + 1L)
    }.sum
  }
}

object Day7 {
  def apply() = new Day7

  case class Hand(val cards: String) {
    def rank(): Int = {
      val counts = mutable.Map[Char, Int]().withDefaultValue(0)
      cards.foreach { c =>
        counts(c) = counts(c) + 1
      }
      if (counts.size == 1) {
        7 // Five of a kind
      } else if (counts.size == 2 && (counts.values.max == 4)) {
        6 // Four of a kind
      } else if (counts.size == 2 && (counts.values.max == 3)) {
        5 // Full house
      } else if (counts.size == 3 && (counts.values.max == 3)) {
        4 // Three of a kind
      } else if (counts.size == 3) {
        3 // Two pair
      } else if (counts.size == 4) {
        2 // One pair
      } else 1 // High Card
    }
  }

  case class WildHand(val cards: String) {
    def rank(): Int = {
      var counts = mutable.Map[Char, Int]().withDefaultValue(0)
      cards.foreach { c =>
        counts(c) = counts(c) + 1
      }
      val nJs = counts('J')
      counts = counts - 'J'
      if (counts.isEmpty || counts.size == 1) {
        7 // Five of a kind
      } else if (counts.size == 2 && (counts.values.max + nJs == 4)) {
        6 // Four of a kind
      } else if (counts.size == 2 && (counts.values.max + nJs == 3)) {
        5 // Full house
      } else if (counts.size == 3 && (counts.values.max + nJs == 3)) {
        4 // Three of a kind
      } else if (counts.size == 3) {
        3 // Two pair
      } else if (counts.size == 4) {
        2 // One pair
      } else 1 // High Card
    }
  }

  case class HandWithBid(hand: Hand, bid: Long)

  case class WildHandWithBid(hand: WildHand, bid: Long)


  implicit val HandOrdering: Ordering[HandWithBid] = new Ordering[HandWithBid] {
    val cardOrder = Array('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

    override def compare(x: HandWithBid, y: HandWithBid): Int = {
      val xR = x.hand.rank()
      val yR = y.hand.rank()
      if (xR != yR) {
        xR - yR
      } else {
        compareCards(x.hand.cards, y.hand.cards)
      }
    }

    @tailrec
    private[this] def compareCards(l: String, r: String): Int = {
      if (l.charAt(0) != r.charAt(0)) {
        cardOrder.indexOf(l.charAt(0)) - cardOrder.indexOf(r.charAt(0))
      } else {
        compareCards(l.tail, r.tail)
      }
    }
  }

  implicit val WildHandOrdering: Ordering[WildHandWithBid] = new Ordering[WildHandWithBid] {
    val cardOrderWild = Array('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')

    override def compare(x: WildHandWithBid, y: WildHandWithBid): Int = {
      val xR = x.hand.rank()
      val yR = y.hand.rank()
      if (xR != yR) {
        xR - yR
      } else {
        compareCards(x.hand.cards, y.hand.cards)
      }
    }

    @tailrec
    private[this] def compareCards(l: String, r: String): Int = {
      if (l.charAt(0) != r.charAt(0)) {
        cardOrderWild.indexOf(l.charAt(0)) - cardOrderWild.indexOf(r.charAt(0))
      } else {
        compareCards(l.tail, r.tail)
      }
    }

  }
}

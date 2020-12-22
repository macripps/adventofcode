package aoc2020

import aoc.Day
import aoc2020.Day22._

import scala.collection.mutable

class Day22 extends Day(2020, 22) {
  val decks: Seq[Seq[Int]] = inputGroups.map { g =>
    g.drop(1).map(_.toInt).toSeq
  }.toSeq

  override def part1: String = {
    var p1Deck = decks.head
    var p2Deck = decks(1)

    while (p1Deck.nonEmpty && p2Deck.nonEmpty) {
      val p1Draw = p1Deck.head
      val p2Draw = p2Deck.head

      if (p1Draw > p2Draw) {
        p1Deck = (p1Deck.tail :+ p1Draw) :+ p2Draw
        p2Deck = p2Deck.tail
      } else {
        p1Deck = p1Deck.tail
        p2Deck = (p2Deck.tail :+ p2Draw) :+ p1Draw
      }
    }

    (score(p1Deck) + score(p2Deck)).toString
  }

  def score(deck: Seq[Int]): Int = {
    deck.reverse.zipWithIndex.map { case (v, idx) => v * (idx + 1) }.sum
  }

  override def part2: String = {
    val (_, p1Deck, p2Deck) = new RecursiveGame(decks.head, decks(1)).play()
    (score(p1Deck) + score(p2Deck)).toString
  }
}

object Day22 {
  def apply() = new Day22()

  class RecursiveGame(p1Deck: Seq[Int], p2Deck: Seq[Int]) {
    val cache = mutable.Set[(Seq[Int], Seq[Int])]()

    def play(): (Winner, Seq[Int], Seq[Int]) = {
      var player1Deck = p1Deck
      var player2Deck = p2Deck
      var winner: Option[Winner] = None
      while (winner.isEmpty) {
        if (cache.contains((player1Deck, player2Deck))) {
          winner = Some(Player1)
        } else {
          cache.add((player1Deck, player2Deck))
          val p1Draw = player1Deck.head
          player1Deck = player1Deck.tail
          val p2Draw = player2Deck.head
          player2Deck = player2Deck.tail

          val w = if (p1Draw <= player1Deck.length && p2Draw <= player2Deck.length) {
            val (w, _, _) = new RecursiveGame(player1Deck.take(p1Draw), player2Deck.take(p2Draw)).play()
            w
          } else {
            if (p1Draw > p2Draw) Player1 else Player2
          }

          w match {
            case Player1 => player1Deck = (player1Deck :+ p1Draw) :+ p2Draw
            case Player2 => player2Deck = (player2Deck :+ p2Draw) :+ p1Draw
          }

          if (player1Deck.isEmpty) {
            winner = Some(Player2)
          } else if (player2Deck.isEmpty) {
            winner = Some(Player1)
          }
        }
      }
      (winner.get, player1Deck, player2Deck)
    }
  }

  trait Winner

  case object Player1 extends Winner

  case object Player2 extends Winner

}

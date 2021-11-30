package aoc2020

import aoc.Day
import Day21._

import scala.collection.mutable
import scala.util.matching.Regex

class Day21 extends Day(2020, 21) {
  override def part1(input: Array[String]): String = {
    val parsedInput = input.map {
      case parse(ingredients, _, allergens, _) =>
        (ingredients.split(" ").toSet, allergens.split(", "))
    }

    val allergensToIngredients = mutable.Map[String, Set[String]]()
    val allIngredients = mutable.Set[String]()

    parsedInput.foreach { case (ingredients, allergens) =>
        allIngredients.addAll(ingredients)

        allergens.foreach { a =>
          if (!allergensToIngredients.contains(a)) {
            allergensToIngredients(a) = ingredients
          } else {
            allergensToIngredients(a) = allergensToIngredients(a).intersect(ingredients)
          }
        }
    }

    val safeIngredients = allIngredients.filter { i =>
      !allergensToIngredients.values.exists(x => x.contains(i))
    }

    val answer = parsedInput.map { case (i, _) =>
      i.count(safeIngredients.contains)
    }.sum

    answer.toString
  }

  override def part2(input: Array[String]): String = {
    val parsedInput = input.map {
      case parse(ins, _, alls, _) =>
        val ingredients = ins.split(" ").toSet
        val allergens = alls.split(", ")
        (ingredients, allergens)
    }

    val allergensToIngredients = mutable.Map[String, Set[String]]()
    val allIngredients = mutable.Set[String]()

    parsedInput.foreach { case (ingredients, allergens) =>
      allIngredients.addAll(ingredients)

      allergens.foreach { a =>
        if (!allergensToIngredients.contains(a)) {
          allergensToIngredients(a) = ingredients
        } else {
          allergensToIngredients(a) = allergensToIngredients(a).intersect(ingredients)
        }
      }
    }

    val safeIngredients = allIngredients.filter { i =>
      !allergensToIngredients.values.exists(x => x.contains(i))
    }

    val imMaybeKnown = allergensToIngredients.map { case (allergen, ingredients) =>
      allergen -> mutable.Set.newBuilder.addAll(ingredients.removedAll(safeIngredients)).result()
    }.toMap

    val maybeKnown = mutable.Map.newBuilder[String, mutable.Set[String]].addAll(imMaybeKnown.toSet).result()

    while (maybeKnown.exists{ case (_, s) => s.size > 1}) {
      maybeKnown.
        filter { case (_, s) => s.size == 1 }.
        foreach { case (a, s) =>
        maybeKnown.keys.filter(_ != a).foreach { k =>
          maybeKnown(k).remove(s.head)
        }
      }
    }

    maybeKnown.keys.toSeq.sorted.map { k => maybeKnown(k).head }.mkString(",")
  }
}

object Day21 {
  def apply() = new Day21()

  val parse: Regex = raw"((\w+ )+)\(contains ((\w,? ?)+)\)".r
}

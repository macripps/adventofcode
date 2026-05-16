package aoc2020

import aoc.NewDay
import Day21._

import scala.collection.mutable
import scala.util.matching.Regex

class Day21 extends NewDay(2020, 21) {
  part(1) {
    execute { in =>
      val parsedInput = in.map {
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
  }

  part(2) {
    execute { in =>
      val parsedInput = in.map {
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
}

object Day21Main extends Day21

object Day21 {
  val parse: Regex = raw"((\w+ )+)\(contains ((\w,? ?)+)\)".r
}

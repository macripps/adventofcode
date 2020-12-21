package aoc2015

import aoc.Day
import Day16._

class Day16 extends Day(2015, 16) {
  override def part1: String = {
    input.map {
      case Parse(s: Sue) => s
    }.filter { s =>
      s.children.isEmpty || s.children.get == 3
    }.filter { s =>
      s.cats.isEmpty || s.cats.get == 7
    }.filter { s =>
      s.samoyeds.isEmpty || s.samoyeds.get == 2
    }.filter { s =>
      s.pomeranians.isEmpty || s.pomeranians.get == 3
    }.filter { s =>
      s.akitas.isEmpty || s.akitas.get == 0
    }.filter { s =>
      s.vizslas.isEmpty || s.vizslas.get == 0
    }.filter { s =>
      s.goldfish.isEmpty || s.goldfish.get == 5
    }.filter { s =>
      s.trees.isEmpty || s.trees.get == 3
    }.filter { s =>
      s.cars.isEmpty || s.cars.get == 2
    }.filter { s =>
      s.perfumes.isEmpty || s.perfumes.get == 1
    }.map(_.num).mkString(",")
  }

  override def part2: String = {
    input.map {
      case Parse(s: Sue) => s
    }.filter { s =>
      s.children.isEmpty || s.children.get == 3
    }.filter { s =>
      s.cats.isEmpty || s.cats.get > 7
    }.filter { s =>
      s.samoyeds.isEmpty || s.samoyeds.get == 2
    }.filter { s =>
      s.pomeranians.isEmpty || s.pomeranians.get < 3
    }.filter { s =>
      s.akitas.isEmpty || s.akitas.get == 0
    }.filter { s =>
      s.vizslas.isEmpty || s.vizslas.get == 0
    }.filter { s =>
      s.goldfish.isEmpty || s.goldfish.get < 5
    }.filter { s =>
      s.trees.isEmpty || s.trees.get > 3
    }.filter { s =>
      s.cars.isEmpty || s.cars.get == 2
    }.filter { s =>
      s.perfumes.isEmpty || s.perfumes.get == 1
    }.map(_.num).mkString(",")
  }
}

object Day16 {
  def apply() = new Day16()

  case class Sue(num: Int,
                 children: Option[Int],
                 cats: Option[Int],
                 samoyeds: Option[Int],
                 pomeranians: Option[Int],
                 akitas: Option[Int],
                 vizslas: Option[Int],
                 goldfish: Option[Int],
                 trees: Option[Int],
                 cars: Option[Int],
                 perfumes: Option[Int],
                )

  object Parse {
    def unapply(line: String): Option[Sue] = {
      val num = line.slice(4, line.indexOf(':')).toInt
      var sue = Sue(num, None, None, None, None, None, None, None, None, None, None)
      val data = line.drop(line.indexOf(':') + 2).split(", ")
      data.foreach {
        case Children(x) => sue = sue.copy(children = Some(x))
        case Cats(x) => sue = sue.copy(cats = Some(x))
        case Samoyeds(x) => sue = sue.copy(samoyeds = Some(x))
        case Pomeranians(x) => sue = sue.copy(pomeranians = Some(x))
        case Akitas(x) => sue = sue.copy(akitas = Some(x))
        case Vizslas(x) => sue = sue.copy(vizslas = Some(x))
        case Goldfish(x) => sue = sue.copy(goldfish = Some(x))
        case Trees(x) => sue = sue.copy(trees = Some(x))
        case Cars(x) => sue = sue.copy(cars = Some(x))
        case Perfumes(x) => sue = sue.copy(perfumes = Some(x))
        case _ =>
      }
      Some(sue)
    }
  }

  object Children {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("children: ")) {
        Some(data.drop(10).toInt)
      } else {
        None
      }
    }
  }

  object Cats {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("cats: ")) {
        Some(data.drop(6).toInt)
      } else {
        None
      }
    }
  }

  object Samoyeds {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("samoyeds: ")) {
        Some(data.drop(10).toInt)
      } else {
        None
      }
    }
  }

  object Pomeranians {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("pomeranians: ")) {
        Some(data.drop(13).toInt)
      } else {
        None
      }
    }
  }

  object Akitas {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("akitas: ")) {
        Some(data.drop(8).toInt)
      } else {
        None
      }
    }
  }

  object Vizslas {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("vizslas: ")) {
        Some(data.drop(9).toInt)
      } else {
        None
      }
    }
  }

  object Goldfish {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("goldfish: ")) {
        Some(data.drop(10).toInt)
      } else {
        None
      }
    }
  }

  object Trees {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("trees: ")) {
        Some(data.drop(7).toInt)
      } else {
        None
      }
    }
  }

  object Cars {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("cars: ")) {
        Some(data.drop(6).toInt)
      } else {
        None
      }
    }
  }

  object Perfumes {
    def unapply(data: String): Option[Int] = {
      if (data.startsWith("perfumes: ")) {
        Some(data.drop(10).toInt)
      } else {
        None
      }
    }
  }

}

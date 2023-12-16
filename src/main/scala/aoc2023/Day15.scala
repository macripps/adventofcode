package aoc2023

import aoc.NewDay

import scala.collection.mutable

class Day15 extends NewDay(2023, 15) {
  part(1) {
    test {
      "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" -> 1320
    }
    execute { input =>
      input.flatMap { line =>
        line.split(',').map { step =>
          hash(step).toLong
        }
      }.sum
    }
  }

  part(2) {
    test {
      "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" -> 145
    }

    execute { input =>
      val boxes = Array.ofDim[mutable.Map[String, Long]](256)
      boxes.indices.foreach { x =>
        boxes(x) = mutable.LinkedHashMap[String, Long]()
      }
      input.flatMap { line =>
        line.split(',').map { step =>
          if (step.last == '-') {
            val label = step.dropRight(1)
            val idx = hash(label)
            boxes(idx) -= label
          } else {
            val Array(label, lens) = step.split('=')
            val idx = hash(label)
            boxes(idx) += (label -> lens.toLong)
          }
        }
      }
      boxes.indices.map { idx =>
        (idx + 1) * boxes(idx).zipWithIndex.map { f: ((String, Long), Int) =>
          (f._2 + 1) * f._1._2
        }.sum
      }.sum
    }
  }

  private[this] def hash(step: String): Int = {
    var hash = 0
    step.foreach { c =>
      hash = hash + c
      hash = hash * 17
      hash = hash % 256
    }
    hash
  }
}

object Day15Main extends Day15

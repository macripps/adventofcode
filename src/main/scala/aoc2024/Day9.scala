package aoc2024

import scala.collection.mutable

class Day9 extends aoc.NewDay(2024, 9) {
  part(1) {
    test(
      """2333133121414131402""".stripMargin -> 1928)
    execute { ls =>
      val input = ls.head.toCharArray.map(_ - '0')
      val output = mutable.Buffer[Long]()
      var id = 0L
      var isFile = true
      input.foreach { x =>
        if (isFile) {
          (0 until x).foreach { _ =>
            output.append(id)
          }
          id = id + 1
        } else {
          (0 until x).foreach { _ =>
            output.append(-1L)
          }
        }
        isFile = !isFile
      }
      var index = input.head
      var last = output.length - 1
      while (index < last) {
        output(index) = output(last)
        output(last) = -1
        while (output(last) == -1) {
          last = last - 1
        }
        while (output(index) != -1) {
          index = index + 1
        }
      }
      output.filter(_ != -1).zipWithIndex.map { case (c, i) => c * i.toLong }.sum
    }
  }

  part(2) {
    test("""2333133121414131402""".stripMargin -> 2858)
    execute { ls =>
      val input = ls.head.toCharArray.map(_ - '0')
      val output = mutable.Buffer[Block]()
      var id = 0L
      var isFile = true
      input.foreach { x =>
        output.append(
          if (isFile) File(id, x) else Free(x)
        )
        if (isFile) {
          id = id + 1
        }
        isFile = !isFile
      }
      var nextId = id - 1
      while (nextId > 0) {
        val blockIdx = output.indexWhere(b => b.isInstanceOf[File] && b.asInstanceOf[File].id == nextId)
        val block = output(blockIdx)
        val maybeFit = output.indices.find { i =>
          i < blockIdx && output(i).isInstanceOf[Free] && output(i).extent >= block.extent
        }
        maybeFit.foreach { idx =>
          output.remove(blockIdx)
          insertFree(output, blockIdx, block.extent)
          val oldSpace = output.remove(idx)
          if (oldSpace.extent > block.extent) {
            insertFree(output, idx, oldSpace.extent - block.extent)
          }
          output.insert(idx, block)
        }
        nextId = nextId - 1
      }
      var checksum = 0L
      var idx = 0L
      output.foreach {
        case Free(x) =>
          idx = idx + x
        case File(id, x) =>
          (0L until x).foreach { _ =>
            checksum = checksum + (idx * id)
            idx = idx + 1
          }
      }
      checksum
    }

    def insertFree(a: mutable.Buffer[Block], idx: Int, extent: Long): Unit = {
      var newExtent = extent
      var insertIdx = idx
      if (insertIdx < a.length && a(insertIdx).isInstanceOf[Free]) {
        val nextFree = a.remove(insertIdx)
        newExtent = newExtent + nextFree.extent
      }
      if (a(insertIdx - 1).isInstanceOf[Free]) {
        val prevFree = a.remove(insertIdx - 1)
        newExtent = newExtent + prevFree.extent
        insertIdx = insertIdx - 1
      }
      a.insert(insertIdx, Free(newExtent))
    }
  }
}

sealed trait Block {
  val extent: Long
}

case class Free(extent: Long) extends Block

case class File(id: Long, extent: Long) extends Block

object Day9Main extends Day9

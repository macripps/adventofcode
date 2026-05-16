package aoc2022

import aoc.NewDay
import aoc2022.Day7.{Directory, File}

import scala.collection.mutable

class Day7 extends NewDay(2022, 7) {
  part(1) {
    execute { in =>
      val (dirs, _) = parseAllDirectories(in)
      val smallDirs = dirs.filter(e => e.size() <= 100000).toSeq
      smallDirs.map(_.size()).sum
    }
  }

  part(2) {
    execute { in =>
      val (dirs, fs) = parseAllDirectories(in)
      val totalSpace = 70000000L
      val minFreeSpace = 30000000L

      val unusedSpace = totalSpace - fs.size()
      dirs.filter(e => e.size() + unusedSpace > minFreeSpace).minBy(_.size()).size()
    }
  }

  private[this] def parseAllDirectories(input: Array[String]) = {
    val dirs = mutable.Set[Directory]()
    val fs = Directory("")
    fs.parent = fs
    var current: Directory = fs
    var listing = false
    input.foreach { line =>
      if (line.startsWith("$ cd ")) {
        listing = false
        val dirName = line.drop(5)
        if (dirName == "..") {
          current = current.parent
        } else if (dirName == "/") {
          current = fs
        } else {
          current = current.entries.find { e =>
            e match {
              case Directory(d) if d == current.name() + "/" + dirName => true
              case _ => false
            }
          }.get.asInstanceOf[Directory]
        }
      } else if (line.startsWith("$ ls")) {
        listing = true
      } else if (listing && line.startsWith("dir ")) {
        val dirName = line.drop(4)
        val d = Directory(current.name() + "/" + dirName)
        d.parent = current
        current.entries.add(d)
        dirs.add(d)
      } else if (listing) {
        val file = line.split(" ")
        current.entries.add(File(file(1), file(0).toLong))
      }
    }
    (dirs, fs)
  }
}

object Day7Main extends Day7

object Day7 {
  trait Entry {
    def size(): Long
    def name(): String
    def show(): Seq[String]
  }

  case class Directory(_name: String) extends Entry {
    var parent: Directory = _
    val entries = mutable.Set[Entry]()

    override def size(): Long = {
      val s = entries.map(_.size()).sum
      s
    }
    override def name(): String = _name

    override def show(): Seq[String] = {
      Seq(
        "- " + _name + s" (dir, size=" + (if(size()<=100000) "\u001B[38;2;57;170;132m" else "") + size() + "\u001B[0m)",
      ) ++ entries.toSeq.sortBy(_.name()).flatMap(e => e.show()).map("  " + _)
    }
  }

  case class File(_name: String, _size: Long) extends Entry {
    override def size(): Long = _size
    override def name(): String = _name
    override def show(): Seq[String] = Seq(
      s"- ${name()} (file, size=${size()})"
    )
  }
}

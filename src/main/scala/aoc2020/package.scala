import scala.io.Source

package object aoc2020 {

  def readFileToIterable(filename: String): Iterable[String] = {
    Source.fromResource(filename).getLines().to(Iterable)
  }
}
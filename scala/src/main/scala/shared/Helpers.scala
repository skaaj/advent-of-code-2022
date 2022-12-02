package shared

import scala.io.Source

object Helpers {

  def loadInput(filename: String): Iterator[String] = {
    Source
      .fromResource(filename)
      .getLines
  }

  def loadInputAsSeq(filename: String): Seq[String] = {
      loadInput(filename).toSeq
  }

  def loadInputAsString(filename: String): String = {
    loadInput(filename).mkString("\n")
  }

  extension[T1, T2] (tuple: Tuple2[T1, T2])
    def first: T1 = tuple._1
    def second: T2 = tuple._2
}

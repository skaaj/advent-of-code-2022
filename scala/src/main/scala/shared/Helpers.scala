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
    def x: T1 = tuple._1
    def y: T2 = tuple._2

  extension[A] (xs: Seq[A])
    def takeTo(p: A => Boolean): Seq[A] =
      xs.span(p) match { case (a, b) => a ++ b.take(1) }
}

package shared

import scala.io.Source

object Helpers {
  def inputAsSeq(filename: String) = {
    Source
      .fromResource(filename)
      .getLines
      .toSeq
  }

  extension[T1, T2] (tuple: Tuple2[T1, T2])
    def first: T1 = tuple._1
    def second: T2 = tuple._2
}

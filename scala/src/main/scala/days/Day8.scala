package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day8 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1() = {
    def visible(heights: Array[Int]) = {
      heights.zipWithIndex.foldLeft((-1, Seq.empty[Int])){
        case ((max, acc), (height, index)) if height > max =>
          (height, index +: acc)
        case (previousState ,_) =>
          previousState
      }.second
    }

    val forest = loadInputAsSeq("input_08.txt")
      .map(_.toCharArray.map(_.toString.toInt))
      .toArray

    val visibleFromLeft = forest.zipWithIndex.flatMap { case (heights, y) =>
      visible(heights).map(x => (x, y))
    }
    val visibleFromRight = forest.zipWithIndex.flatMap { case (heights, y) =>
      visible(heights.reverse).map(x => (heights.length - (x + 1), y))
    }
    val visibleFromTop = forest.transpose.zipWithIndex.flatMap { case (heights, x) =>
      visible(heights).map(y => (x, y))
    }
    val visibleFromBottom = forest.transpose.zipWithIndex.flatMap { case (heights, x) =>
      visible(heights.reverse).map(y => (x, heights.length - (y + 1)))
    }

    (visibleFromLeft ++ visibleFromRight ++ visibleFromTop ++ visibleFromBottom)
      .toSet
      .size
  }

  def part2() = {
    val forest = loadInputAsSeq("input_08.txt")
      .map(_.toCharArray.map(_.toString.toInt))
      .toArray

    def viewingDistance(heights: Seq[Int], start: Int) = {
      val startHeight = heights(start)
      val toRight = heights.drop(start + 1).takeTo(_ < startHeight).length
      val toLeft = heights.take(start).reverse.takeTo(_ < startHeight).length
      // when input is transposed
      //    toLeft -> toTop
      //    toRight -> toBottom
      Seq(toLeft, toRight)
    }

    val horizontalDistances = for {
      y <- 0 until forest.length
      x <- 0 until forest(y).length
    } yield ((x, y), viewingDistance(forest(y), x))
    val rotatedForest = forest.transpose
    val verticalDistances = for {
      x <- 0 until rotatedForest.length
      y <- 0 until rotatedForest(x).length
    } yield ((y, x), viewingDistance(rotatedForest(y), x))

    (horizontalDistances ++ verticalDistances)
      .groupBy(_.first)
      .mapValues(_.flatMap(_.second).foldLeft(1)(_ * _))
      .maxBy(_.second).second
  }
}

package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.{Success, Try}
import scala.util.matching.Regex
import org.json4s.*
import org.json4s.jackson.Serialization.read

object Day13 {
  implicit val formats: Formats = DefaultFormats

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def parse(filename: String): Seq[(Seq[Any], Seq[Any])] = {
    loadInputAsString(filename)
      .split("\n\n")
      .map(_.split("\n").map(read[Seq[Any]](_)))
      .map({ case Array(first, second) => (first, second) })
  }

  def compare(left: Any, right: Any): Option[Boolean] = {
    (left, right) match {
      case (left: BigInt, right: BigInt) =>
        if(left != right) Some(left < right)
        else None
      case (left: BigInt, right: Seq[_]) =>
        compare(Seq(left), right)
      case (left: Seq[_], right: BigInt) =>
        compare(left, Seq(right))
      case (left: Seq[_], right: Seq[_]) =>
        val zipped = left.zip(right)
        val result = zipped.foldLeft(None: Option[Boolean]) {
          case (None, (a, b)) => compare(a, b)
          case (result @ Some(_), _) => result
        }.orElse {
          if(left.length == right.length) None
          else Some(left.length < right.length)
        }
        result
    }
  }

  def part1() = {
    val parsed = parse("input_13.txt")
    val result = parsed.zipWithIndex.map {
      case ((left, right), index) =>
        if(compare(left, right).getOrElse(true)) index + 1 else 0
    }
    result.sum
  }

  def part2() = {
    val parsed = parse("input_13.txt")
    val dividerTwo = Seq(Seq(BigInt(2)))
    val dividerSix = Seq(Seq(BigInt(6)))
    val ordered = (parsed
      .unzip match { case (xs, ys) => xs ++ ys ++ Seq(dividerTwo, dividerSix) })
      .sortWith((a, b) => compare(a, b).getOrElse(true))
    val dividerTwoIndex = ordered.indexOf(dividerTwo) + 1
    val dividerSixIndex = ordered.indexOf(dividerSix) + 1
    dividerTwoIndex * dividerSixIndex
  }
}

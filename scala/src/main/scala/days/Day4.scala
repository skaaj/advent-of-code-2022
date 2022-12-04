package days

import shared.Helpers.*

import scala.io.Source

object Day4 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1() = {
    loadInput("input_04.txt")
      .map(_.split(",").map(_.split("-")))
      .map {
        case Array(Array(a, b), Array(c, d)) =>
          ((a.toInt, b.toInt), (c.toInt, d.toInt))
      }
      .filter {
        case ((a, b), (c, d)) =>
          (a >= c && b <= d) || (c >= a && d <= b)
      }
      .length
  }

  def part2() = {
    loadInput("input_04.txt")
      .map(_.split(",").map(_.split("-")))
      .map {
        case Array(Array(a, b), Array(c, d)) =>
          ((a.toInt, b.toInt), (c.toInt, d.toInt))
      }
      .filter {
        case ((a, b), (c, d)) =>
          b >= c && a <= d
      }
      .length
  }
}

package days

import shared.Helpers.*

import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  val priorities = (('a' to 'z') ++ ('A' to 'Z'))
    .zipWithIndex
    .toMap
    .mapValues(_ + 1)

  def part1() = {
    loadInput("input_03.txt")
      .map(line => line.splitAt(line.length / 2))
      .map {
        case (first, second) =>
          second
            .find(char => first.contains(char))
            .flatMap(char => priorities.get(char))
      }
      .flatten
      .sum
  }

  def part2() = {

    loadInput("input_03.txt")
      .grouped(3)
      .map {
        case Seq(first, second, third) =>
          first
            .find(char => second.contains(char) && third.contains(char))
            .flatMap(char => priorities.get(char))
      }
      .flatten
      .sum
  }
}

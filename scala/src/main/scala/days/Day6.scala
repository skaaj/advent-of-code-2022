package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day6 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def solve(filename: String, n: Int) = {
    val input = loadInputAsString(filename)

    val ngrams = for {
      i <- n until input.length
    } yield input.slice(i - n, i)

    val ngramIndex = ngrams.indexWhere { ngram =>
      ngram.toSeq.size == ngram.toSet.size
    }

    ngramIndex + n
  }

  def part1() = {
    solve("input_06.txt", 4)
  }

  def part2() = {
    solve("input_06.txt", 14)
  }
}

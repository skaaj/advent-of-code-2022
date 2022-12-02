package days

import shared.Helpers.*

import scala.io.Source

object Day2 {

  enum HandSign(val score: Int):
    case Rock extends HandSign(1)
    case Paper extends HandSign(2)
    case Scissors extends HandSign(3)

  enum Outcome(val score: Int):
    case Win extends Outcome(6)
    case Draw extends Outcome(3)
    case Lose extends Outcome(0)

  val leftCode = Map(
    "A" -> HandSign.Rock,
    "B" -> HandSign.Paper,
    "C" -> HandSign.Scissors
  )

  val strategy = Map(
    "A" -> "Y",
    "B" -> "X",
    "C" -> "Z"
  )

  def getRoundResult(left: HandSign, right: HandSign) = {
    // right biaised, we are calculating right player score (left is the opponent)
    ((left, right) match {
      case (HandSign.Rock, HandSign.Paper) => Outcome.Win.score
      case (HandSign.Scissors, HandSign.Rock) => Outcome.Win.score
      case (HandSign.Paper, HandSign.Scissors) => Outcome.Win.score
      case _ => if(left == right) Outcome.Draw.score else Outcome.Lose.score
    }) + right.score
  }

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Int = {
    val rightCode = Map(
      "X" -> HandSign.Rock,
      "Y" -> HandSign.Paper,
      "Z" -> HandSign.Scissors
    )
    loadInput("input_02.txt").foldLeft(0: Int) {
      case (acc, line) =>
        line.split(" ").toSeq match {
          case Seq(left: String, right: String) =>
            acc + getRoundResult(leftCode(left), rightCode(right))
        }
    }
  }

  def part2(): Int = {
    val rightCode = Map(
      "X" -> Outcome.Lose,
      "Y" -> Outcome.Draw,
      "Z" -> Outcome.Win
    )
    val beatPairs = Seq(
      HandSign.Rock -> HandSign.Scissors,
      HandSign.Paper -> HandSign.Rock,
      HandSign.Scissors -> HandSign.Paper
    )
    loadInput("input_02.txt")
      .map(line => line.split(" ").toSeq)
      .map {
        case Seq(rawLeft, rawRight) => (leftCode(rawLeft), rightCode(rawRight))
      }
      .map {
        case (left, Outcome.Draw) => (left, Some(left))
        case (left, Outcome.Win) => (left, beatPairs.find(_._2 == left).map(_._1))
        case (left, Outcome.Lose) => (left, beatPairs.find(_._1 == left).map(_._2))
      }
      .map {
        case (left, Some(right)) => getRoundResult(left, right)
        case _ => throw new Exception("malformed input ?")
      }
      .sum
  }
}

package days

import scala.io.Source
import shared.Helpers.*

object Day1 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def part1(): Int = {
    (inputAsSeq("input_1.txt") :+ "")
      .foldLeft((0, Int.MinValue)){
        // accumulator is (current group sum, max so far)
        case ((currentSum, max), "") =>
          (0, if(currentSum > max) currentSum else max)
        case ((currentSum, max), line) =>
          (currentSum + line.toInt, max)
      }
      .second
  }

  def part2(): Int = {
    val desc = Ordering[Int].reverse
    (inputAsSeq("input_1.txt") :+ "")
    .foldLeft((0, Seq.empty[Int])){
      // accumulator is (current group sum, top 3 so far)
      case ((currentSum, top), "") =>
        (0, (currentSum +: top).sorted(desc).take(3))
      case ((currentSum, top), line) =>
        (currentSum + line.toInt, top)
    }
    .second
    .sum
  }
}

package days

import shared.Helpers.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day10 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def parse(filename: String): Seq[Int] = {
    loadInputAsSeq(filename).flatMap { instruction =>
      instruction.split(" ") match {
        case Array("noop") => Seq(0)
        case Array("addx", value) => Seq(0, value.toInt)
      }
    }
  }

  def getRegisterHistory(xs: Seq[Int]): Seq[Int] = {
    xs.scanLeft(1)(_ + _)
  }

  def part1() = {
    val deltaPerCycle = parse("input_10_sample.txt")
    val registerHistory = getRegisterHistory(deltaPerCycle)

    Seq(20, 60, 100, 140, 180, 220)
      .map(cycle => cycle * registerHistory(cycle - 1))
      .sum
  }

  def part2() = {
    val deltaPerCycle = parse("input_10.txt")
    val registerHistory = getRegisterHistory(deltaPerCycle)
    val width = 40
    val height = 6
    for(y <- 0 until height) {
      for(x <- 0 until width) {
        val register = registerHistory((y * width) + x)
        val isAbove = x >= (register - 1) && (x <= register + 1)
        print(if(isAbove) "#" else ".")
      }
      println()
    }
  }
}

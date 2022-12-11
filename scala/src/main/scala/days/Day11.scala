package days

import shared.Helpers.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day11 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  case class Monkey(
    items: Seq[Long],
    operation: Long => Long,
    divisibility: Long,
    whenTrue: Int,
    whenFalse: Int
  )

  def extractInfos(line: String) = {
    if (line.contains("Starting"))
      line.split(":")(1).trim
    else if (line.contains("Operation"))
      line.split("=")(1).trim
    else if (line.contains("Test"))
      line.split("by")(1).trim
    else if (line.contains("If true") || line.contains("If false"))
      line.split("throw to monkey")(1).trim
    else line
  }

  def infosToMonkey(infos: Array[String]) = infos match {
    case Array(items, operation, divisibility, whenTrue, whenFalse) =>
      val operationFunc: Long => Long =
        operation.split(" ") match {
          case Array(_, "*", "old") =>
            (old) => old * old
          case Array(_, "*", right) =>
            _ * right.toLong
          case Array(_, "+", "old") =>
            (old) => old + old
          case Array(_, "+", right) =>
            _ + right.toLong
        }

      Monkey(
        items.split(", ").map(_.toLong),
        operationFunc,
        divisibility.toLong,
        whenTrue.toInt,
        whenFalse.toInt
      )
  }

  def parse(filename: String) = {
    loadInputAsString(filename)
      .split("\n\n")
      .map(_.split("\n").drop(1).map(extractInfos))
      .map(infosToMonkey)
      .toSeq
  }

  def evalRound(monkeys: Seq[Monkey], relief: Long => Long): Seq[(Monkey, Long)] = {
    val received = List.fill(monkeys.length)(mutable.ArrayBuffer[Long]())
    val inspections = mutable.ArrayBuffer(List.fill(monkeys.length)(0):_*)

    for {
      (monkey, i) <- monkeys.zipWithIndex
    } {
      val allItems = monkey.items ++ received(i)
      for(item <- allItems) {
        val worry = relief(monkey.operation(item))
        val target = {
          if(worry % monkey.divisibility == 0)
            monkey.whenTrue
          else
            monkey.whenFalse
        }
        received(target).addOne(worry)
      }
      received(i).clear()
      inspections(i) = allItems.length
    }

    for {
      (monkey, i) <- monkeys.zipWithIndex
    } yield (monkey.copy(items = received(i).toSeq), inspections(i))
  }

  def part1() = {
    val monkeys = parse("input_11_sample.txt")
    val inspections = mutable.ArrayBuffer[(Long, Long)]()
    val finalState = (0 until 20).foldLeft(monkeys) {
      case (prev, _) =>
        evalRound(prev, _ / 3L).zipWithIndex.map {
          case ((monkey, count) ,i) =>
            inspections.addOne((i, count))
            monkey
        }
    }
    val inspectionsCount = inspections
      .groupBy(_.first)
      .map(_.second.map(_.second).sum)
      .toSeq
      .sortBy(-_)
    inspectionsCount(0) * inspectionsCount(1)
  }

  def part2() = {
    val monkeys = parse("input_11_sample.txt")
    val inspections = mutable.ArrayBuffer[(Long, Long)]()
    val modulo = monkeys.map(_.divisibility).foldLeft(1L)(_ * _)
    val finalState = (0 until 10000).foldLeft(monkeys) {
      case (prev, _) =>
        evalRound(prev, _ % modulo).zipWithIndex.map {
          case ((monkey, count) ,i) =>
            inspections.addOne((i, count))
            monkey
        }
    }
    val inspectionsCount = inspections
      .groupBy(_.first)
      .map(_.second.map(_.second).sum)
      .toSeq
      .sortBy(-_)
    inspectionsCount(0) * inspectionsCount(1)
  }
}

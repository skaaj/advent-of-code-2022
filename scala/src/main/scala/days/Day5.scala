package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day5 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  case class Operation(source: Int, dest: Int, amount: Int)

  def parse(filename: String): (Seq[mutable.Stack[String]], Seq[Operation]) = {
    // separate crates from operations (two strings)
    val (rawCrates, rawOperation) =
      loadInputAsString(filename).split("\n\n") match {
        case Array(rawCrates, rawOperation) => (rawCrates, rawOperation)
      }

    // -- handle raw crates
    val (header, body) = rawCrates
      .split("\n")
      .reverse
      .partition(_.startsWith(" 1"))
    // find the number of stacks to create
    val numOfStack = header
      .flatMap(line => line.replace("   ", " ").split(" "))
      .map(x => Try(x.toInt).toOption)
      .flatten
      .max
    val stacks = Seq.tabulate(numOfStack)(_ => mutable.Stack.empty[String])
    // extract the crates to be put in the stacks
    // from bottom to top (to make insertion easier)
    val cratesByLevel = for {
      line <- body
      toExtract = (1 to line.length by 4)
    } yield toExtract.map(i => line(i))
    // perform insertion
    for {
      level <- cratesByLevel
      (crate, i) <- level.map(_.toString).zipWithIndex
      if crate != " "
    } stacks(i).push(crate)

    // -- handle operations
    val pattern = "^move ([0-9]*) from ([0-9]*) to ([0-9]*)$".r
    val operations = rawOperation
      .split("\n")
      .map { line =>
        pattern.findFirstMatchIn(line).map { item =>
          Operation(item.group(2).toInt - 1, item.group(3).toInt - 1, item.group(1).toInt)
        }
      }
      .flatten

    (stacks, operations)
  }

  def part1() = {
    val (stacks, operations) = parse("input_05.txt")
    // apply the operations
    for {
      operation <- operations
      _ <- 0 until operation.amount
    } {
      val crate = stacks(operation.source).pop()
      stacks(operation.dest).push(crate)
    }
    // fetch all top crates
    (for {
      stack <- stacks
    } yield stack.top).mkString
  }

  def part2() = {
    val (stacks, operations) = parse("input_05.txt")
    // apply the operations
    for {
      operation <- operations
    } {
      val crates = stacks(operation.source).popAll()
      stacks(operation.dest).pushAll(crates.take(operation.amount).reverse)
      stacks(operation.source).pushAll(crates.drop(operation.amount).reverse)
    }
    // fetch all top crates
    (for {
      stack <- stacks
    } yield stack.top).mkString
  }
}

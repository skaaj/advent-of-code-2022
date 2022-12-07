package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day7 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  def parse(filename: String) = {
    def isCd(line: String) = line.startsWith("$ cd")
    def isLs(line: String) = line.startsWith("$ ls")
    def getCdArg(line: String) = line.drop(5)

    def iter(lines: Seq[String], currentPath: Seq[String], sizes: Seq[(String, Long)]): Seq[(String, Long)] =
      lines.headOption.fold(sizes){ line =>
        val nextLines = lines.drop(1)
        if(isCd(line)) {
          getCdArg(line) match {
            case ".." =>
              iter(nextLines, currentPath.drop(1), sizes)
            case directory =>
              iter(nextLines, directory +: currentPath, sizes)
          }
        } else {
          if(isLs(line)) {
            iter(nextLines, currentPath, sizes)
          } else {
            line.split(" ") match {
              case Array("dir", _) => iter(nextLines, currentPath, sizes)
              case Array(size, filename) =>
                val newSizes = currentPath
                  .reverse
                  .scan("")({ case (a, b) => f"$a/$b" })
                  .drop(1)
                  .map(directory => (directory, size.toLong))
                iter(nextLines, currentPath, sizes ++ newSizes)
            }
          }
        }
      }
    iter(loadInputAsSeq(filename), Seq.empty, Seq.empty)
  }

  def part1() = {
    val directorySizes = parse("input_07.txt")
      .groupBy(_._1)
      .mapValues(_.map(_._2).reduce(_ + _))
      .toMap

    directorySizes.values.filter(_ <= 100_000).sum
  }

  def part2() = {
    val directorySizes = parse("input_07.txt")
      .groupBy(_._1)
      .mapValues(_.map(_._2).reduce(_ + _))
      .toMap
    val totalSpace = 70000000
    val usedSpace = directorySizes.maxBy(_.second).second
    val freeSpace = totalSpace - usedSpace
    val spaceNeeded = 30000000 - freeSpace

    directorySizes
      .values.toSeq
      .filter(_ >= spaceNeeded)
      .sorted
      .head
  }
}

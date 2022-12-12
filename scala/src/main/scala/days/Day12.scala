package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day12 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  val elevationsMagnitudes = ('a' to 'z').zipWithIndex.toMap

  sealed trait Cell(val elevation: Int)
  case class Start() extends Cell(0)
  case class Finish() extends Cell(elevationsMagnitudes('z'))
  case class Road(override val elevation: Int) extends Cell(elevation)

  type Point = (Int, Int)

  def parse(filename: String): Array[Array[Cell]] = {
    loadInputAsSeq(filename)
      .map(_.toCharArray.map {
        case 'S' => Start()
        case 'E' => Finish()
        case char => Road(elevationsMagnitudes(char))
      })
      .toArray
  }

  def findInGrid(
    grid: Array[Array[Cell]],
    pred: Cell => Boolean
  ): Seq[(Point, Cell)] = {
    for {
      y <- 0 until grid.length
      x <- 0 until grid(y).length
      if pred(grid(y)(x))
    } yield ((x, y), grid(y)(x))
  }

  def findShortest(start: Point, end: Point, edges: Map[Point, Seq[Point]]) = {
    def go(remaining: Seq[(Point, Int)], done: Set[Point]): Int = {
      def hasRemainingEdges(point: Point): Boolean = {
        val res = edges.get(point).toSet
          .diff(done)
          .size > 0
        res
      }

      remaining.sortBy(_.second) match {
        case (current, distance) :: tail if current == end =>
          distance
        case (current, distance) :: Seq() if !hasRemainingEdges(current) =>
          distance
        case (current, _) :: tail if done.contains(current) =>
          go(tail, done)
        case (current, distance) :: tail =>
          go(tail ++ edges.getOrElse(current, Seq.empty).map((_, distance + 1)), done + current)
      }
    }

    go(Seq((start, 0)), Set.empty)
  }

  def part1() = {
    val grid = parse("input_12.txt")
    val (height, width) = (grid.length, grid(0).length)
    val start = findInGrid(grid, _.isInstanceOf[Start]).head.first
    val finish = findInGrid(grid, _.isInstanceOf[Finish]).head.first

    val edges = for {
      y <- 0 until grid.length
      x <- 0 until grid(y).length
      b <- Math.max(y - 1, 0) until Math.min(y + 2, grid.length)
      a <- Math.max(x - 1, 0) until Math.min(x + 2, grid(y).length)
      if (x, y) != (a, b) // no move to itself
      if (x != a && y == b) || (y != b && x == a) // no move in diagonal
      if grid(b)(a).elevation <= grid(y)(x).elevation + 1
    } yield ((x, y), (a, b))

    val groupedEdges = edges
      .groupBy(_.first)
      .mapValues(_.map(_.second))
      .toMap

    findShortest(start, finish, groupedEdges)
  }

  def part2() = {
    val grid = parse("input_12.txt")
    val (height, width) = (grid.length, grid(0).length)
    val start = findInGrid(grid, _.isInstanceOf[Start]).head.first
    val finish = findInGrid(grid, _.isInstanceOf[Finish]).head.first

    val edges = for {
      y <- 0 until grid.length
      x <- 0 until grid(y).length
      b <- Math.max(y - 1, 0) until Math.min(y + 2, grid.length)
      a <- Math.max(x - 1, 0) until Math.min(x + 2, grid(y).length)
      if (x, y) != (a, b) // no move to itself
      if (x != a && y == b) || (y != b && x == a) // no move in diagonal
      if grid(b)(a).elevation <= grid(y)(x).elevation + 1
    } yield ((x, y), (a, b))

    val groupedEdges = edges
      .groupBy(_.first)
      .mapValues(_.map(_.second))
      .toMap

    val xs = findInGrid(grid, _.elevation == 0)
      .map(_.first)
      .map(start => findShortest(start, finish, groupedEdges))
    ???
  }
}

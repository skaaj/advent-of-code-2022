package days

import shared.Helpers.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import scala.annotation.tailrec

object Day09 {
  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  type Point = (Int, Int)

  def getDistance(a: Point, b: Point): Double = {
    val (ax, ay) = a
    val (bx, by) = b
    Math.sqrt(Math.pow(Math.abs(ax-bx), 2) + Math.pow(Math.abs(ay-by), 2))
  }

  def add(a: Point, b: Point): Point = {
    (a.first + b.first, a.second + b.second)
  }

  def subtract(a: Point, b: Point): Point = {
    (a.first - b.first, a.second - b.second)
  }

  def printBoard(head: Point, tail: Point): Unit = {
    for {
      y <- 0 until 6
    } {
      for {
        x <- 0 until 6
      } {
        if((x,y) == head) print("H")
        else if((x,y) == tail) print("T")
        else print(".")
      }
      println()
    }
  }

  def printBoard2(head: Point, tail: Seq[Point]): Unit = {
    for {
      y <- 25 to 0 by -1
    } {
      for {
        x <- -25 until 25
      } {
        if((x,y) == head) print("H")
        else if(tail.contains((x,y))) print("T")
        else print(".")
      }
      println()
    }
  }

  def deltaFromName(name: String): (Int, Int) =
    name match {
      case "R" => (1, 0)
      case "L" => (-1, 0)
      case "U" => (0, 1)
      case "D" => (0, -1)
      case _ => throw new Exception("malformed input")
    }

  def part1() = {
    val parsed = loadInputAsSeq("input_09.txt")
      .map(_.split(" "))
      .map({ case Array(dir, mag) => (dir, mag.toInt) })

    @tailrec
    def go(
      head: Point,
      tail: Point,
      moves: Seq[(String, Int)],
      visited: Set[Point]
    ): Set[Point] = {
      moves match {
        case Seq() =>
          visited
        case (dir, mag) :: nextMoves =>
          printBoard(head, tail)
          val delta = deltaFromName(dir)
          val newHead = add(head, delta)
          val distance = getDistance(newHead, tail)
          val newTail = {
            if(distance < 1.5f) tail
            else {
              val diff = subtract(head, tail)
              add(tail, diff)
            }
          }
          val newMoves = if(mag == 1) nextMoves else (dir, mag - 1) +: nextMoves
          go(newHead, newTail, newMoves, visited + newTail)
      }
    }

    val result = go(
      head = (0, 0),
      tail = (0, 0),
      moves = parsed,
      visited = Set((0, 0))
    )

    result.size
  }

  def part2() = {
    val parsed = loadInputAsSeq("input_09.txt")
      .map(_.split(" "))
      .map({ case Array(dir, mag) => (dir, mag.toInt) })

    def normalize(x: Int): Int =
      x / Math.abs(x)

    def updateTail(head: Point, tail: Seq[Point]): Seq[Point] = {
      tail.foldLeft((head, Seq.empty[Point])) {
        case ((prev, updated), knot) =>
          val (dx, dy) = (prev.x - knot.x, prev.y - knot.y)
          val (mx, my) = (dx, dy) match {
            case (a, b) if Math.abs(a) <= 1 && Math.abs(b) <= 1 =>
              (0, 0)
            case (a, b) if a == 0 =>
              (0, normalize(b))
            case (a, b) if b == 0 =>
              (normalize(a), 0)
            case (a, b) =>
              (normalize(a), normalize(b))
          }
          val newKnot = (knot.x + mx, knot.y + my)
          (newKnot, updated :+ newKnot)
      }.second
    }

    @tailrec
    def go(
      head: Point,
      tail: Seq[Point],
      moves: Seq[(String, Int)],
      visited: Set[Point]
    ): Set[Point] = {
      moves match {
        case Seq() =>
          visited
        case (dir, mag) :: nextMoves =>
          printBoard2(head, tail)
          val delta = deltaFromName(dir)
          val newHead = add(head, delta)
          val newTail = updateTail(newHead, tail)
          val newMoves = if(mag == 1) nextMoves else (dir, mag - 1) +: nextMoves
          println()
          go(newHead, newTail, newMoves, visited + newTail.last)
      }
    }

    val result = go(
      head = (0, 0),
      tail = (0 until 9).map(_ => (0, 0)),
      moves = parsed,
      visited = Set((0, 0))
    )

    result.size
  }
}

package days

import org.json4s.*
import org.json4s.jackson.Serialization.read
import shared.Helpers.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Day14 {
  implicit val formats: Formats = DefaultFormats

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }

  type Vec2d = (Int, Int)
  def addVec2d(a: Vec2d, b: Vec2d) =
    (a.x + b.x, a.y + b.y)

  def parse(filename: String): Seq[Seq[Vec2d]] = {
    loadInputAsSeq(filename)
      .map(
        _.split(" -> ")
          .map(
            _.split(",") match {
              case Array(x, y) => (x.toInt, y.toInt)
            }
          )
      )
  }

  def getRocksFromTraces(traces: Seq[Seq[Vec2d]]): Set[Vec2d] = {
    traces.flatMap { trace =>
      for {
        i <- 0 until trace.length - 1
        start = trace(i)
        end = trace(i + 1)
        dx = if(start.x < end.x) 1 else -1
        dy = if(start.y < end.y) 1 else -1
        x <- start.x to end.x by dx
        y <- start.y to end.y by dy
      } yield (x, y)
    }.toSet
  }


  def findObstacleBelow(spawn: Vec2d, obstacles: Seq[Vec2d]): Option[Vec2d] = {
    obstacles
      .filter(_.y > spawn.y)
      .filter(_.x == spawn.x)
      .sortBy(_.y)
      .headOption
  }

  def getNextSandPosition(spawn: Vec2d, obstacles: Set[Vec2d]): Option[Vec2d] = {
    // find blocked position below
    // try to slide on the left
    // try to slide on the right
    // if left and right are KO it is settled
    // otherwise try again from that new position
    // /!\ WHEN SEARCHING FOR POSITION BELOW
    // if there are no obstacle below it's game over
    val down = findObstacleBelow(spawn, obstacles.toSeq).map(addVec2d(_, (0, -1)))
    down match {
      case Some(position) =>
        val left = {
          val candidate = (position.x - 1, position.y + 1)
          if(!obstacles.contains(candidate)) Some(candidate)
          else None
        }
        val right = {
          val candidate = (position.x + 1, position.y + 1)
          if(!obstacles.contains(candidate)) Some(candidate)
          else None
        }
        left.orElse(right) match {
          case Some(slided) => getNextSandPosition(slided, obstacles)
          case None => Some(position)
        }
      case None =>
        println("game over")
        None
    }
  }

  def draw(rocks: Set[Vec2d], sand: Set[Vec2d], bounds: (Int, Int, Int, Int)): Unit = {
    for {
      y <- 0 until bounds._4 + 10
    } {
      println()
      for {
        x <- bounds._1 - 10 until bounds._2 + 10
      } {
        if(rocks.contains((x,y))) print("#")
        else if(sand.contains((x,y))) print("o")
        else if(x == 500 && y == 0) print("s")
        else print(".")
      }
    }
    println()
  }

  def part1() = {
    val traces = parse("input_14.txt")
    val rocks = getRocksFromTraces(traces)
    val sandSource = (500, 0)

    val minX = rocks.minBy(_.x).x - 1
    val maxX = rocks.maxBy(_.x).x + 1
    val minY = rocks.minBy(_.y).y - 1
    val maxY = rocks.maxBy(_.y).y + 1

    def go(obstacles: Set[Vec2d]): Int = {
      getNextSandPosition(sandSource, obstacles) match {
        case Some(position) =>
          //draw(rocks, (obstacles + position).diff(rocks), (minX, maxX, minY, maxY))
          go(obstacles + position)
        case None =>
          obstacles.size - rocks.size
      }
    }

    val result = go(rocks)
    result
  }

  def part2() = {
    val traces = parse("input_14.txt")
    val rocks = getRocksFromTraces(traces)
    val sandSource = (500, 0)

    val minX = rocks.minBy(_.x).x
    val maxX = rocks.maxBy(_.x).x
    val minY = rocks.minBy(_.y).y
    val maxY = rocks.maxBy(_.y).y

    val rocksWithBottom = rocks ++ (for {
      x <- -5_000 until 5_000
    } yield (x, maxY + 2))

    def go(obstacles: Set[Vec2d]): Int = {
      getNextSandPosition(sandSource, obstacles) match {
        case Some((500, 0)) =>
          println("yeah finised")
          1 + (obstacles.size - rocksWithBottom.size)
        case Some(position) =>
          //println(position)
          //draw(rocksWithBottom, (obstacles + position).diff(rocks), (minX, maxX, minY, maxY))
          go(obstacles + position)
        case None =>
          throw new Exception("we fell of")
      }
    }

    val result = go(rocksWithBottom)
    result
  }
}

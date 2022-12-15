package days

import days.Day05.Operation
import org.json4s.*
import org.json4s.jackson.Serialization.read
import shared.Helpers.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Day15 {
  implicit val formats: Formats = DefaultFormats

  def main(args: Array[String]): Unit = {
    println(time(part1()))
    println(time(part2()))
  }

  type Vec2d = (Long, Long)

  case class Sensor(pos: Vec2d, beacon: Vec2d, range: Long) {
    def isInRange(other: Vec2d): Boolean = {
      manDistance(pos, other) <= range
    }
  }

  def addVec2d(a: Vec2d, b: Vec2d) =
    (a.x + b.x, a.y + b.y)

  def subtractVec2d(a: Vec2d, b: Vec2d) =
    (a.x - b.x, a.y - b.y)

  def manDistance(a: Vec2d, b: Vec2d) =
    subtractVec2d(a, b) match {
      case (x, y) => Math.abs(x) + Math.abs(y)
    }

  def parse(filename: String) = {
    val lines = loadInputAsSeq(filename)
    val pattern = "^Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)$".r
    lines.map { line =>
      pattern.findFirstMatchIn(line).map { result =>
        val sx = result.group(1).toLong
        val sy = result.group(2).toLong
        val bx = result.group(3).toLong
        val by = result.group(4).toLong
        val sensor = (sx, sy)
        val beacon = (bx, by)
        Sensor(sensor, beacon, manDistance(sensor, beacon))
      }
    }.flatten
  }

  def getCellsWithoutBeacon(targetY: Long, sensors: Seq[Sensor]): Seq[Vec2d] = {
    // compute collisions segments
    val collisionSegments = sensors.flatMap {
      case Sensor(sp @ (sx, sy), bp @ (bx, by), range) =>
        val overflow = range - Math.abs(sy - targetY)
        // negative overflow -> no collision
        // zero overflow -> collision on one cell
        // positive overflow -> collision multiple cells
        if(overflow >= 0) {
          Seq((sx - overflow, sx + overflow))
        } else {
          Seq.empty[Vec2d]
        }
    }
    // merge them together if needed
    @tailrec
    def go(prev: Vec2d, created: Seq[Vec2d], remaining: Seq[Vec2d]): Seq[Vec2d] = {
      remaining.headOption match {
        case Some(item) =>
          if(item.first <= prev.second) {
            val newSegment = (
              Math.min(item.first, prev.first),
              Math.max(item.second, prev.second)
            )
            go(newSegment, created, remaining.drop(1))
          } else {
            go(item, prev +: created, remaining.drop(1))
          }
        case None =>
          prev +: created
      }
    }

    collisionSegments.sortBy(_.first) match {
      case head :: tail if tail.nonEmpty =>
        go(head, Seq.empty, tail)
      case xs =>
        xs
    }
  }

  def part1() = {
    val sensors = parse("input_15.txt")
    val segments = getCellsWithoutBeacon(targetY = 2000000, sensors)
    segments.map(segment => segment.second - segment.first).sum
  }

  def part2() = {
    val sensors = parse("input_15.txt")
    val maxBound = 4000000

    val segmentsByLine = for {
      y <- (0 to maxBound).view
      segments = getCellsWithoutBeacon(y, sensors)
      if segments.length > 1
    } yield (y, segments)

    segmentsByLine.headOption.map {
      case (y, segments) if segments.nonEmpty =>
        val x = segments.sortBy(_.first).map(_.second)(0)
        (4000000 * x) + y
    }.getOrElse(-1)
  }
}

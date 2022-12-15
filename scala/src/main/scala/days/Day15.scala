package days

import days.Day05.Operation
import org.json4s.*
import org.json4s.jackson.Serialization.read
import shared.Helpers.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Success, Try}

object Day15 {
  implicit val formats: Formats = DefaultFormats

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
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

  def getCellsWithoutBeacon(boundsX: Option[(Long, Long)], targetY: Long, sensors: Seq[Sensor]): Long = {
    val minX = boundsX.map(_.first).getOrElse(
      sensors.map(sensor => Math.min(sensor.pos.x - sensor.range, sensor.beacon.x)).min
    )
    val maxX = boundsX.map(_.second).getOrElse(
      sensors.map(sensor => Math.max(sensor.pos.x + sensor.range, sensor.beacon.x)).max
    )
    val sensorsReachingTarget = sensors.filter(sensor =>
      sensor.range >= Math.abs(sensor.pos.y - targetY)
    )

    var counter = 0
    for {
      x <- minX to maxX
      y = targetY
      if sensorsReachingTarget.exists(sensor =>
        sensor.beacon != (x, y) && sensor.isInRange((x, y))
      )
    } counter += 1

    counter
  }

  def part1() = {
    val sensors = parse("input_15.txt")
    getCellsWithoutBeacon(None, 2000000, sensors)
  }

  def part2() = {
    ???
  }
}

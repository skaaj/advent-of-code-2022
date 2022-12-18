package days

import days.Day16.Valve
import shared.Helpers.*
import sun.security.provider.certpath.AdjacencyList

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day18 {
  def main(args: Array[String]): Unit = {
    println(time(part1()))
    println(time(part2()))
  }

  case class Cube(name: String, pos: Vector3d)
  case class Vector3d(x: Int, y: Int, z: Int) {
    def manhattanWith(other: Vector3d) =
      Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
  }

  def parse(filename: String): Seq[Cube] = {
    val lines = loadInput(filename)
    val pattern = "(\\d+),(\\d+),(\\d+)".r
    lines.zipWithIndex.map {
      case (pattern(x, y, z), i) =>
        Cube(s"C$i", Vector3d(x.toInt, y.toInt, z.toInt))
    }.toSeq
  }

  def getAdjacencyMap(points: Seq[Vector3d], edges: Seq[(Vector3d, Vector3d)]): Map[Vector3d, Seq[Vector3d]] = {
    points match {
      case Seq(head, tail@_*) if tail.nonEmpty =>
        val newEdges = tail
          .filter(_.manhattanWith(head) <= 1)
          .flatMap(other => Seq(head -> other, other -> head))
        getAdjacencyMap(tail, edges ++ newEdges)
      case _ =>
        edges
          .groupBy(_.first)
          .mapValues(_.map(_.second))
          .toMap
    }
  }

  def gatherAdjacentVoxels(start: Vector3d, adjacencyMap: Map[Vector3d, Seq[Vector3d]]): Seq[Vector3d] = {
    val queue = mutable.Queue[Vector3d]()
    val visited = mutable.HashSet[Vector3d]()
    visited.add(start)
    queue.enqueue(start)
    while(queue.nonEmpty) {
      val voxel = queue.dequeue()
      for(child <- adjacencyMap(voxel) if !visited.contains(child)) {
        visited.add(child)
        queue.enqueue(child)
      }
    }
    visited.toSeq
  }

  def part1() = {
    val cubes = parse("input_18_sample.txt")
    val adjacencyMap = getAdjacencyMap(cubes.map(_.pos), Seq.empty)
    val covered = adjacencyMap.values.map(_.length).sum
    (6 * cubes.length) - covered
  }

  def part2() = {
    // Idea here, not very fast but should do the job:
    // - find a box around the shape
    // - start an exploration from a point we know is not in the shape
    // - gather all the air voxel accessible from the starting point
    // - for each air voxel collected check how many rock it is touching
    val rocks = parse("input_18.txt")
    val rocksPositions = rocks.map(_.pos)
    val xs = rocksPositions.map(_.x)
    val ys = rocksPositions.map(_.y)
    val zs = rocksPositions.map(_.z)
    val boxVoxels = for {
      z <- (zs.min - 1) to (zs.max + 1)
      y <- (ys.min - 1) to (ys.max + 1)
      x <- (xs.min - 1) to (xs.max + 1)
      vec3 = Vector3d(x, y, z)
      if !rocksPositions.contains(vec3)
    } yield Vector3d(x, y, z)

    val outsideAir = gatherAdjacentVoxels(boxVoxels.head, getAdjacencyMap(boxVoxels, Seq.empty))

    val covered = (for {
      airVoxel <- outsideAir
      rockVoxel <- rocksPositions
      if(airVoxel.manhattanWith(rockVoxel) <= 1)
    } yield 1).sum

    covered
  }
}

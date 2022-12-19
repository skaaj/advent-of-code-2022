package days

import shared.Helpers.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day16 {
  def main(args: Array[String]): Unit = {
    println(time(part1()))
    println(time(part2()))
  }

  type Path = Seq[String]
  case class Valve(name: String, rate: Int, sinks: Seq[String])

  def parse(filename: String) = {
    val lines = loadInputAsSeq(filename)
    val pattern = "Valve ([A-Z]{2}) has flow rate=(\\d+); (?:tunnel leads to valve|tunnels lead to valves) (.+)".r
    lines.map {
      case pattern(name, rate, rawSinks) =>
        val sinks = rawSinks.split(", ").toSeq
        (name, Valve(name, rate.toInt, sinks))
    }.toMap
  }

  def getShortestPaths(start: Valve)(implicit ref: Map[String, Valve]): Map[String, Int] = {
    def go(parent: String, children: Seq[String], cost: Int, checked: Set[String], state: Map[String, Int]): Map[String, Int] = {
      children.toList match {
        case Seq() if checked.size < state.size =>
          // updating current vertex
          val selected = state
            .filterKeys(k => !checked.contains(k))
            .minBy(_.second)
          val newParent = ref(selected.first)
          val newChildren = newParent.sinks
          go(newParent.name, newParent.sinks, selected.second + 1, checked + parent, state)
        case currentNode :: nextNodes =>
          // inspecting adjacent nodes
          val currentCost = state(currentNode)
          val newState =
            if(currentCost > cost) state + (currentNode -> cost)
            else state
          go(parent, nextNodes, cost, checked, newState)
        case _ =>
          state
      }
    }

    val allNodes = ref.keys
    val validNodes = ref.filter(_.second.rate > 0)
    val initialState = allNodes.map((_, Int.MaxValue)).toMap + (start.name -> 0)
    go(start.name, start.sinks, 1, Set.empty, initialState)
      .filterKeys(validNodes.contains)
      .filter({ case (_, cost) => cost > 0 })
      .toMap
  }

  def computeScore(rate: Int, timeLeft: Int): Int = {
    Math.max(timeLeft, 0) * rate
  }

  def computeOutcomes(
    start: String,
    costs: Map[String, Map[String, Int]],
    totalTime: Int,
    toVisit: Set[String]
  )(implicit ref: Map[String, Valve]) = {
    def go(
      current: String,
      path: Path,
      score: Int,
      remaining: Set[String],
      timeLeft: Int
    ): Seq[(Path, Int)] = {
      if(remaining.isEmpty) Seq((path, score))
      else {
        remaining.foldLeft(Seq.empty[(Path, Int)]) {
          case (paths, target) =>
            val newTimeLeft = timeLeft - costs(current)(target) - 1
            if(newTimeLeft < 1) {
              paths ++ Seq((path, score))
            } else {
              val rate = ref(target).rate
              val newScore = score + computeScore(rate, newTimeLeft)
              paths ++ go(target, path :+ target, newScore, remaining - target, newTimeLeft)
            }
        }
      }
    }

    go(start, Seq(start), 0, toVisit, totalTime)
  }

  def part1() = {
    implicit val ref = parse("input_16.txt")

    val totalTime = 30
    val validTargets = ref.values.toSeq.filter(_.rate > 0)

    val costs =
      (ref("AA") +: validTargets).map(valve =>
        (valve.name, getShortestPaths(ref(valve.name)))
      ).toMap

    computeOutcomes("AA", costs, totalTime, validTargets.map(_.name).toSet)
      .maxBy(_.second)
  }

  def part2() = {
    implicit val ref = parse("input_16.txt")

    val totalTime = 26
    val validTargets = ref.values.toSeq.collect {
      case valve if valve.rate > 0 => valve.name
    }

    val costsFromStart = ("AA", getShortestPaths(ref("AA")))
    val costsFromOthers =
      validTargets.map(valve =>
        (valve, getShortestPaths(ref(valve)))
      ).toMap
    val costs = costsFromOthers + costsFromStart

    val outcomes = for {
      areaCenter <- validTargets
      areaRadius <- 1 until validTargets.length
      humanTargets = costs(areaCenter).toSeq
        .sortBy(_.second)
        .map(_.first)
        .take(areaRadius)
      humanPass = computeOutcomes("AA", costs, totalTime, humanTargets.toSet + areaCenter)
        .maxBy(_.second)
      elephantTargets = validTargets.toSet.diff(humanPass.first.toSet)
      elephantPass = computeOutcomes("AA", costs, totalTime, elephantTargets)
        .maxBy(_.second)
    } yield (humanPass, elephantPass, humanPass.second + elephantPass.second)

    outcomes.maxBy(_._3)
  }
}

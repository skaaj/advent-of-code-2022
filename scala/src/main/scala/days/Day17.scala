package days

import days.Day17.SeqGrid
import shared.Helpers.*

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.util.matching.Regex

object Day17 {
  def main(args: Array[String]): Unit = {
    println(time(part1()))
    println(time(part2()))
  }

  type SeqGrid[T] = Seq[Seq[T]]
  type Position = (Long, Long)
  type JetPattern = LazyList[Char]

  case class Rock(position: Position, mask: SeqGrid[Boolean], label: String) {
    val right = position.x + mask(0).length
    val top = position.y + mask.length
    val left = position.x
    val bottom = position.y
    def at(newPosition: Position) = this.copy(position = newPosition)
    def atX(x: Long) = this.copy(position = (x, position.y))
    def atY(y: Long) = this.copy(position = (position.x, y))
    def collideWith(other: Rock): Boolean = {
      if(bottom > other.top || top < other.bottom) false
      else if(left > other.right || right < other.left) false
      else {
        (for {
          my <- (0 until mask.length).view
          mx <- 0 until mask(my).length
          if(mask(my)(mx))
          omy <- 0 until other.mask.length
          omx <- 0 until other.mask(omy).length
          if(other.mask(omy)(omx))
          y = position.y + my
          x = position.x + mx
          oy = other.position.y + omy
          ox = other.position.x + omx
        } yield x == ox && y == oy).exists(identity)
      }
    }
    def repr: String = s"$label(${position.x})"
  }

  def parseRocks(filename: String): Seq[Rock] = {
    loadInputAsString(filename)
      .split("\n\n")
      .zipWithIndex
      .map { (lines, i) =>
        val mask = lines.split("\n")
          .map(_.toCharArray.map(_ == '#').toSeq)
          .reverse
          .toSeq
        Rock((0, 0), mask, ('A' to 'Z').toSeq(i).toString)
      }
      .toSeq
  }

  def loopPattern[T](pattern: Seq[T]): LazyList[T] = {
    val length = pattern.length
    LazyList.from(0).map(i => pattern(i % length))
  }

  def applyGravity(rock: Rock, grid: Seq[Rock]): Option[Rock] = {
    val movedRock = rock.atY(rock.bottom - 1)
    if(movedRock.bottom < 0) None
    else {
      val isColliding = (
        for {
          restingRock <- grid.view
        } yield movedRock.collideWith(restingRock)
      ).exists(identity)
      if(isColliding) None else Some(movedRock)
    }
  }

  def applyJet(rock: Rock, jet: Char, grid: Seq[Rock]): Option[Rock] = {
    val dx = if(jet == '<') -1 else 1
    val movedRock = rock.atX(rock.position.x + dx)
    if(movedRock.left < 0 || movedRock.right > 7) None
    else {
      val isColliding = (
        for {
          restingRock <- grid.view
        } yield movedRock.collideWith(restingRock)
      ).exists(identity)
      if(isColliding) None else Some(movedRock)
    }
  }

  def part1() = {
    val jetPattern = loopPattern(loadInputAsString("input_17_sample.txt"))
    val rocksPattern = loopPattern(parseRocks("input_17_rocks.txt"))
    val rockPool = rocksPattern.take(2022)

    def settleRock(rock: Rock, grid: Seq[Rock]): (Option[Rock], JetPattern) = {
      @tailrec
      def go(currentRock: Rock, currentJetPattern: JetPattern): (Option[Rock], JetPattern) = {
        val movedByJet = applyJet(currentRock, currentJetPattern.head, grid)
          .orElse(Some(currentRock))
        val movedByGravity = movedByJet.flatMap(rock => applyGravity(rock, grid))
        movedByGravity match {
          case Some(movedRock) =>
            go(movedRock, currentJetPattern.drop(1))
          case None =>
            (movedByJet, currentJetPattern.drop(1))
        }
      }
      go(rock, jetPattern)
    }

    def placeRocks(): Seq[Rock] =  {
      rockPool.foldLeft((Seq.empty[Rock], jetPattern)) { case ((grid, jetPattern), rock) =>
        val highestInTheRoom = grid
          .maxByOption(_.top)
          .map(_.top)
          .getOrElse(0L)
        val spawnedRock = rock.at((2, highestInTheRoom + 3))
        val (settledRockOpt, newJetPattern) = settleRock(spawnedRock, grid)
        (grid ++ settledRockOpt, newJetPattern)
      }.first
    }

    val completedGrid = placeRocks()
    val highestInTheRoom = completedGrid
      .maxByOption(_.top)
      .map(_.top)
      .getOrElse(-1)
    highestInTheRoom
  }

  def part2() = {
    // First we want to detect where cycles are and what properties they have.
    //
    // In order to do that we can launch the simulation for a "tracing" run.
    // Every iteration state is saved and compared with the past ones.
    // If it matches then it means that we are starting a new iteration of that cycle.
    // Then we just have to use that information to identify cycles properties.
    //
    // From that we now know the heights increases:
    // - from start to 1st cycle
    // - for all the cycling part
    // The only missing piece is the one after the cycles.
    // To find it we can launch the simulation for a run.
    //
    // Finally by adding start + cycles + end we should get the result !

    val jetPattern = loadInputAsString("input_17.txt")
    val rocksPattern = parseRocks("input_17_rocks.txt")
    val totalIteration = 1_000_000_000_000L

    case class IterationState(grid: Seq[Rock], rockCount: Long, jetCount: Long) {
      def repr: String = {
        val rockOffset = (rockCount % rocksPattern.length).toInt
        val jetOffset = (jetCount % jetPattern.length).toInt
        s"$rockOffset#$jetOffset#${grid.map(_.repr).mkString}"
      }
    }

    def getGridHeight(grid: Seq[Rock]): Long = {
      grid.maxByOption(_.top).map(_.top).getOrElse(0L)
    }

    @tailrec
    def fall(rock: Rock, grid: Seq[Rock], jetCount: Long): (Option[Rock], Long) = {
      val jetOffset = (jetCount % jetPattern.length).toInt
      val movedByJet = applyJet(rock, jetPattern(jetOffset), grid).orElse(Some(rock))
      val movedByGravity = movedByJet.flatMap(rock => applyGravity(rock, grid))
      movedByGravity match {
        case Some(movedRock) =>
          fall(movedRock, grid, jetCount + 1)
        case None =>
          (movedByJet, jetCount + 1)
      }
    }

    def spawnRock(grid: Seq[Rock], rockCount: Long): Rock = {
      val rockOffset = (rockCount % rocksPattern.length).toInt
      rocksPattern(rockOffset)
        .copy()
        .at((2L, 3L + getGridHeight(grid)))
    }

    def updateGrid(grid: Seq[Rock], rockToAddOpt: Option[Rock], size: Int = 49): Seq[Rock] = {
      val truncatedGrid = if(grid.length >= size) grid.take(size) else grid
      rockToAddOpt.toSeq ++ truncatedGrid
    }

    def detectCycle(maxIterations: Int = 10_000): Option[(IterationState, IterationState)] = {
      def iterate(grid: Seq[Rock], rockCount: Long, jetCount: Long, states: Map[String, IterationState]): Option[(IterationState, IterationState)] = {
        if(rockCount < maxIterations) {
          val state = IterationState(grid, rockCount, jetCount)
          val stateKey = state.repr
          states.get(stateKey) match {
            case Some(oldState) =>
              Some((oldState, state))
            case None =>
              val spawnedRock = spawnRock(grid, rockCount)
              val (settledRock, newJetCount) = fall(spawnedRock, grid, jetCount)
              val newGrid = updateGrid(grid, settledRock)
              iterate(newGrid, rockCount + 1, newJetCount, states + (stateKey -> state))
          }
        } else {
          None
        }
      }

      iterate(Seq.empty, 0, 0, Map.empty)
    }

    def getLastPartHeight(iterationState: IterationState, remainingCount: Long): Long = {
      val grid = iterationState.grid
      val jetCount = iterationState.jetCount
      val rockCount = iterationState.rockCount
      val range = (rockCount until (rockCount + remainingCount))
      val (finalGrid, _) = range.foldLeft((grid, jetCount)) { case ((grid, jetCount), rockCount) =>
        val spawnedRock = spawnRock(grid, rockCount)
        val (settledRock, newJetCount) = fall(spawnedRock, grid, jetCount)
        (updateGrid(grid, settledRock), newJetCount)
      }
      getGridHeight(finalGrid) - getGridHeight(grid)
    }

    detectCycle().map { case (oldState, newState) =>
      val cycleStart = oldState.rockCount
      val cycleSize = newState.rockCount - oldState.rockCount
      val cycleCount = (totalIteration - cycleStart) / cycleSize
      val remainingRocks = (totalIteration - cycleStart) % cycleSize
      val startHeight = getGridHeight(oldState.grid)
      val cycleHeightDelta = getGridHeight(newState.grid) - startHeight
      val cyclesTotalHeightDelta = cycleCount * cycleHeightDelta
      val endHeight = getLastPartHeight(oldState, remainingRocks)
      startHeight + cyclesTotalHeightDelta + endHeight
    }.getOrElse(-1)
  }
}

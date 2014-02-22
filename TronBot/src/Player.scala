import scala.collection.mutable
import scala.io.Source

object Player {

  val GameWidth = 30
  val GameHeight = 20

  val ReallyBadScore = -1000000

  def main(args: Array[String]) {

    val gameGrid = new GameGrid(GameWidth, GameHeight)

    var timer = System.currentTimeMillis()

    var moveCounter = 0

    while (true) {

      // Impure input stuff
      val turnInput: List[String] = readOneTurn(Source.stdin)

      timer = System.currentTimeMillis()

      val myPlayerNumber = turnInput.head.split(" ")(1).toInt

      val playerInfosThisTurn: List[PlayerInfo] = PlayerInfo.getPlayerInformation(turnInput)
      gameGrid.update(playerInfosThisTurn)


      // Pure stuff

      val availableMoves: Set[Move] = gameGrid.getAvailableMoves(myPlayerNumber)

      if (availableMoves.isEmpty) {
        println("HALT")
      } else {

        val playerLocations: mutable.Map[Int, Coordinate] = new mutable.HashMap[Int, Coordinate]()
        for (playerInfo <- playerInfosThisTurn) {
          playerInfo match {
            case PlayerLocation(playerNumber, location, _) => playerLocations += playerNumber -> location
            case _ => ()
          }
        }

        val locationMap: Map[Int, Coordinate] = (for (x: (Int, Coordinate) <- playerLocations) yield x).toMap

        val moveScorePairs: Set[(Move, Int)] = MoveAnalyser.determineMoveGoodness(gameGrid.array, locationMap, myPlayerNumber, moveCounter).toSet

        val maxScore = (for ((_, score) <- moveScorePairs) yield score).max
        val bestMoves: Set[Move] = for ((move, score) <- moveScorePairs if score == maxScore) yield move

        val chosenMove = bestMoves.head

        // Impure output stuff

        writeMove(chosenMove)

        moveCounter += 1

        debug("Processed move in " + (System.currentTimeMillis() - timer) + " ms.")

      }

    }
  }

  def readOneTurn(source: Source): List[String] = {

    val linesIterator: Iterator[String] = source.getLines()

    val firstLine: String = linesIterator.next()

    val numbers: Array[String] = firstLine.split(" ")
    assert(numbers.length == 2)

    val numberOfLinesToFollow = numbers.head.toInt

    val remainingLines: List[String] = (for (i <- 1 to numberOfLinesToFollow) yield linesIterator.next()).toList

    firstLine :: remainingLines

  }

  def writeMove(move: Move): Unit = {
    println(move.toString)
  }

  def debug(message: String): Unit = {
    Console.err.println(message)
  }
}

object MoveAnalyser {

  def extractMyUtility(playerNumber: Int, playerUtilities: Map[Int, Int]): Int = {

    val opponents = playerUtilities.keySet - playerNumber

    val myUtility = playerUtilities(playerNumber)

    val opponentUtilities = for (opponent <- opponents) yield playerUtilities(opponent)

    myUtility - opponentUtilities.sum

  }

  /**
   * Estimate the utility of each move. Only legal moves are returned.
   * @param playerArray a 2D array organised as a list of rows, in which each cell contains either a player number or
   *                    GameGrid.EmptySpaceNumber if the cell contains empty space
   * @param playerLocationMap the location of each player in the array, a cell at coordinate (X,Y) can be access with
   *                          playerArray(y)(x)
   * @param currentPlayer the number of the player whose moves we are considering
   * @return a map from each legal move to an integer estimate of its utility to the currentPlayer
   */
  def determineMoveGoodness(playerArray: Array[Array[Int]], playerLocationMap: Map[Int, Coordinate],
                            currentPlayer: Int, moveCounter: Int): Map[Move, Int] = {

    def getOutcomeUtility(preMoveLocationMap: Map[Int, Coordinate], outcome: MoveSequenceOutcome): Int = {
      outcome match {
        case EndedUpAtLocation(location, resultantArray) =>
          val updatedLocationMap = (preMoveLocationMap - currentPlayer) + (currentPlayer -> location)
          val playerToReachableCount: Map[Int, Int] =
            GridRacer2.getPlayerReachableCounts(updatedLocationMap, (currentPlayer :: (preMoveLocationMap.keySet - currentPlayer).toList).toVector, resultantArray)
          extractMyUtility(currentPlayer, playerToReachableCount)
        case _ => Player.ReallyBadScore
      }
    }

    val playerLocation = playerLocationMap(currentPlayer)

    val moveSequencesOfLengthTwo: Set[List[Move]] = MoveSequenceGenerator.generateMoveSequences(2)

    val moveSequenceResults: Map[List[Move], MoveSequenceOutcome] =
      (for (moveSequence <- moveSequencesOfLengthTwo)
        yield moveSequence -> MoveSequencer.computeSequenceResult(playerArray, playerLocation, moveSequence)).toMap


    val moveSequenceUtilities: Map[List[Move], Int] =
      (for (moveSequence <- moveSequenceResults.keys) yield moveSequence -> getOutcomeUtility(playerLocationMap, moveSequenceResults(moveSequence))).toMap

    // Collect utilities

    val maxUtilities = mutable.Map[Move, Int]()

    for (moveSequence <- moveSequenceUtilities.keys) {
      val move = moveSequence.head
      val utility = moveSequenceUtilities(moveSequence)
      if (!maxUtilities.contains(move) || maxUtilities(move) < utility) {
        maxUtilities(move) = utility
      }
    }

    // Filter out the awful moves
    (for (move <- maxUtilities.keys if maxUtilities(move) > Player.ReallyBadScore) yield (move -> maxUtilities(move))).toMap
  }

}

object GridRacer2 {

  /**
   * Get a map of player number to the number of cells that player can reach before all opponents
   * @param playersAndLocations a map containing players and their locations
   * @param playerTurnOrder a list specifying the order in which players will move, if you care about that (otherwise
   *                        just use playersAndLocations.keySet)
   * @param grid a game grid
   * @return a map of player number to the number of cells that player can reach before all opponents
   */
  def getPlayerReachableCounts(playersAndLocations: Map[Int, Coordinate], playerTurnOrder: Vector[Int], grid: Array[Array[Int]]): Map[Int, Int] = {

    val width = grid.head.length
    val height = grid.length

    val playerCount: Int = playerTurnOrder.size

    val proximityCounts = new Array[Int](playerCount)

    val raceGrid: Array[Array[Int]] = Array.fill(height, width)(GameGrid.EmptySpaceNumber)

    var currentPlayerIndex = 0

    val coordinatesToExplore = new mutable.HashMap[Int, mutable.HashSet[Coordinate]]()

    // Set the current locations of the players
    for (player <- playerTurnOrder) {
      val location: Coordinate = playersAndLocations(player)
      raceGrid(location.y)(location.x) = player

      if (!coordinatesToExplore.contains(player)) {
        coordinatesToExplore(player) = new mutable.HashSet[Coordinate]()
      }

      coordinatesToExplore(player).add(location)
    }

    var exhaustedPlayerCount = 0

    while (exhaustedPlayerCount < playerCount) {

      if (currentPlayerIndex == 0) {
        exhaustedPlayerCount = 0
      }

      val currentPlayer = playerTurnOrder(currentPlayerIndex)

      val playerIterationCoordinates: mutable.HashSet[Coordinate] = coordinatesToExplore(currentPlayer)

      val allNewCellsThisIteration = new mutable.HashSet[Coordinate]()

      for (location <- playerIterationCoordinates) {
        val explorableAdjacents = for(candidate <- location.getAdjacents
                                      if candidate.isWithin(width, height) &&
                                        grid(candidate.y)(candidate.x) == GameGrid.EmptySpaceNumber) yield candidate

        for (cell <- explorableAdjacents) {
          grid(cell.y)(cell.x) = currentPlayer
          allNewCellsThisIteration.add(cell)
        }

        proximityCounts(currentPlayerIndex) += explorableAdjacents.size

      }

      if (playerIterationCoordinates.isEmpty) {
        exhaustedPlayerCount += 1
      }

//      playerIterationCoordinates.clear()
      coordinatesToExplore(currentPlayer) = allNewCellsThisIteration

      // Now we've done the iteration for the current player

      currentPlayerIndex = (currentPlayerIndex + 1) % playerCount

    }

    (for (i <- 0 until playerCount) yield playerTurnOrder(i) -> proximityCounts(i)).toMap

  }

}

class TurnInfo()

class GameGrid(width: Int, height: Int, arr: Array[Array[Int]]) {

  val array = arr

  private val playerLocations = mutable.Map[Int, Coordinate]()

  def this(width: Int, height: Int) = {
    this(width, height, Array.fill(height, width)(GameGrid.EmptySpaceNumber))
  }

  def this(s: String) = {
    this(GameGrid.getDimensions(s)._1, GameGrid.getDimensions(s)._2, ArrayUtils.arrayFromParsing(s))

  }

  /**
   * Update the grid using the player information yielded for the latest turn.
   * @param playerInformation information indicating the location / announcing the death of each player
   */
  def update(playerInformation: List[PlayerInfo]) {

    assert(array.head.length == width)
    assert(array.length == height)

    for(info <- playerInformation) {
      info match {
        case PlayerLocation(playerNumber, Coordinate(x,y), _) =>
          playerLocations(playerNumber) = Coordinate(x,y)
          array(y)(x) = playerNumber
        case PlayerDead(playerNumber) => purgePlayer(playerNumber)
      }
    }
  }

  def purgePlayer(playerNumber: Int): Unit = {
    for(i <- 0 until width; j <- 0 until height) {
      val cell: Int = array(j)(i)
      if (cell == playerNumber) {
        array(j)(i) = GameGrid.EmptySpaceNumber
      }
    }
  }

  def rendered: String = {
    ArrayUtils.render(array)
  }

  def getAvailableMoves(playerNumber: Int): Set[Move] = {

    val playerLocation = playerLocations(playerNumber)

    def cellIsValid(coordinate: Coordinate): Boolean = {
      val (x, y) = (coordinate.x, coordinate.y)
      x < width && x >= 0 &&
        y < height && y >= 0 &&
        array(y)(x) == GameGrid.EmptySpaceNumber
    }

    def moveIsAvailable(move: Move): Boolean = {
      val resultantLocation: Coordinate = playerLocation.applyMove(move)
      cellIsValid(resultantLocation)
    }

    for (move <- Move.AllMoves if moveIsAvailable(move)) yield move

  }

}


object MoveSequenceGenerator {

  /**
   * Generate move sequences that could be beneficial to the current player. Looks further ahead in certain sequences,
   * for example a sequence of moves in a particular direction, or comprising a diagonal ladder.
   * @param maximumNumberOfMoves the maximum length for the generated move sequences
   * @return
   */
  def generateMoveSequences(maximumNumberOfMoves: Int): Set[List[Move]] = {

    val set: Set[List[Move]] = for (move1 <- Move.AllMoves; move2 <- Move.AllMoves) yield List(move1, move2)

    set
  }

}

abstract class MoveSequenceOutcome()

object MoveSequenceOutcome {
  def allMovesIllegal(outcomes: List[MoveSequenceOutcome]): Boolean = {
    for (outcome <- outcomes) {
      outcome match {
        case PerformedIllegalMove() => ()
        case _ => return false
      }
    }
    true
  }
}

sealed case class PerformedIllegalMove() extends MoveSequenceOutcome
sealed case class EndedUpAtLocation(location: Coordinate, resultantArray: Array[Array[Int]]) extends MoveSequenceOutcome

object MoveSequencer {

  import ArrayUtils._

  val ArbitraryPlayerNumber: Int = 0

  def computeSequenceResult(playerArray: Array[Array[Int]], startingLocation: Coordinate,
                            moveSequence: List[Move]): MoveSequenceOutcome = {

    val width = playerArray.head.length
    val height = playerArray.length

    val simulationArray: Array[Array[Int]] = for (cells <- playerArray) yield cells.clone()
    var currentLocation = startingLocation

    simulationArray(startingLocation.y)(startingLocation.x) = ArbitraryPlayerNumber

    for (move <- moveSequence) {
      currentLocation = currentLocation.applyMove(move)

      if (!currentLocation.isWithin(width, height) ||
        get(simulationArray, currentLocation) != GameGrid.EmptySpaceNumber) {
        return PerformedIllegalMove()
      }

      set(simulationArray, currentLocation, ArbitraryPlayerNumber)

    }

    EndedUpAtLocation(currentLocation, simulationArray)
  }

}

object GameGrid {
  val EmptySpaceNumber = -1

  def getDimensions(s: String): (Int, Int) = {
    val lines: List[String] = s.lines.toList
    val firstLine: String = lines.head
    val row: Array[String] = for (cell <- firstLine.split(" ") if !cell.isEmpty) yield cell

    val h = lines.size
    val w = row.size
    (w, h)
  }


}

object ArrayUtils {
  def render(array: Array[Array[Int]]) : String = {

    def padInt(x: Int) = {
      val string: String = x.toString
      if (string.length == 1) " " + string else string
    }
    val paddedCells: Array[Array[String]] = for (row <- array) yield for (cell <- row) yield padInt(cell)

    (for (row <- paddedCells) yield row.mkString(" ")).mkString("\n")
  }

  def get(array: Array[Array[Int]], x: Int, y: Int) = array(y)(x)

  def get(array: Array[Array[Int]], coordinate: Coordinate) = {
    array(coordinate.y)(coordinate.x)
  }

  def set(array: Array[Array[Int]], coordinate: Coordinate, value: Int) = {
    array(coordinate.y)(coordinate.x) = value
  }

  def makeRow(rowString: String): Array[Int] = {
    val cells: Array[String] = for (cell <- rowString.split(" ") if !cell.isEmpty) yield cell
    for (cell <- cells) yield cell.toInt
  }

  def arrayFromParsing(s: String): Array[Array[Int]] = {
    (for (line <- s.lines.toList) yield makeRow(line)).toArray
  }
}

sealed case class Coordinate(x: Int, y: Int) {
  def applyMove(move: Move): Coordinate = {
    move match {
      case Left() => Coordinate(x - 1, y)
      case Right() => Coordinate(x + 1, y)
      case Up() => Coordinate(x, y - 1)
      case Down() => Coordinate(x, y + 1)
    }
  }

  def getAdjacents: List[Coordinate] = {
    List(
      Coordinate(x - 1, y),
      Coordinate(x + 1, y),
      Coordinate(x, y - 1),
      Coordinate(x, y + 1)
    )
  }

  def isWithin(width: Int, height: Int) = {
    x >= 0 && x < width && y >= 0 && y < height
  }

}

abstract sealed class PlayerInfo()

case class PlayerLocation(playerNumber: Int, head: Coordinate, tail: Coordinate) extends PlayerInfo

case class PlayerDead(playerNumber: Int) extends PlayerInfo

object PlayerInfo {

  def getPlayerInformation(turnText: List[String]): List[PlayerInfo] = {
    val playerInfoLines = turnText.tail

    val firstLine: Array[String] = turnText.head.split(" ")
    assert(firstLine.size == 2 && playerInfoLines.size == firstLine.head.toInt)

    val zipped: List[(String, Int)] = playerInfoLines.zip(0 until playerInfoLines.size)

    for((playerLine, playerNumber) <- zipped) yield parse(playerLine, playerNumber)
  }

  def parse(string: String, playerNumber: Int): PlayerInfo = {
    val values = string.split(" ")

    if (values(0) == "-1") {
      PlayerDead(playerNumber)
    } else {
      val numbers = for(value <- values) yield value.toInt
      PlayerLocation(
        playerNumber = playerNumber,
        head = Coordinate(numbers(2), numbers(3)),
        tail = Coordinate(numbers(0), numbers(1)))
    }
  }

}

object Move {
  val AllMoves: Set[Move] = Set(Up(), Down(), Left(), Right())
}

sealed abstract class Move {

  def nextRotation: Move = {
    this match {
      case Left() => Up()
      case Up() => Right()
      case Right() => Down()
      case Down() => Left()
    }
  }

  def reverse: Move = {
    this match {
      case Left() => Right()
      case Up() => Down()
      case Right() => Left()
      case Down() => Up()
    }
  }
}

case class Left() extends Move {
  override def toString = "LEFT"
}
case class Right() extends Move {
  override def toString = "RIGHT"
}
case class Up() extends Move {
  override def toString = "UP"
}
case class Down() extends Move {
  override def toString = "DOWN"
}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Player {

  val GameWidth = 30
  val GameHeight = 20


  def main(args: Array[String]) {

    val gameGrid = new GameGrid(GameWidth, GameHeight)

    while (true) {

      // Impure input stuff
      val turnInput: List[String] = readOneTurn(Source.stdin)

      val myPlayerNumber = turnInput.head.split(" ")(1).toInt

      val playerInfosThisTurn: List[PlayerInfo] = PlayerInfo.getPlayerInformation(turnInput)
      gameGrid.update(playerInfosThisTurn)


      // Pure stuff

      val turn = playerInfosThisTurn(myPlayerNumber)

      val myLocation = turn match {
        case PlayerLocation(_, location, _) => location
        case _ => sys.error("I seem to be dead.")
      }

      val availableMoves: Set[Move] = gameGrid.getAvailableMoves(myPlayerNumber)

      if (availableMoves.isEmpty) {
        println("I seem to have got trapped :(")
      } else {
        // For now, just see which move provides us with the best score
        val distanceFinder = new DistanceFinder(gameGrid.array)

        // For each available move, compute the score
        val moveScores: Set[(Move, Int)] = for (move <- availableMoves) yield (move, distanceFinder.getNumberOfReachableCells(myLocation.applyMove(move)))

        val maxScore = (for ((_, score) <- moveScores) yield score).max
        val bestMoves: Set[Move] = for ((move, score) <- moveScores if score == maxScore) yield move

        val chosenMove = bestMoves.head

        // Impure output stuff

        writeMove(chosenMove)
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

class TurnInfo()

// Try to hide use of Ints
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

  def makeRow(rowString: String): Array[Int] = {
    val cells: Array[String] = for (cell <- rowString.split(" ") if !cell.isEmpty) yield cell
    for (cell <- cells) yield cell.toInt
  }

  def arrayFromParsing(s: String): Array[Array[Int]] = {
    (for (line <- s.lines.toList) yield makeRow(line)).toArray
  }
}

class DistanceFinder(arr: Array[Array[Int]]) {
  val array = arr.clone()

  def getDistanceGridForPlayer(playerLocation: Coordinate): Array[Array[Int]] = {

    val width = array.head.length
    val height = array.length

    val distanceArray: Array[Array[Int]] = Array.fill(height, width)(GameGrid.EmptySpaceNumber)
    val coordinatesToExplore = ArrayBuffer[Coordinate]()

    distanceArray(playerLocation.y)(playerLocation.x) = 0
    coordinatesToExplore.prepend(playerLocation)

    while (!coordinatesToExplore.isEmpty) {
      val next = coordinatesToExplore.remove(0)

      val adjacents = for(candidate <- next.getAdjacents if candidate.isWithin(width, height)) yield candidate
      val distances = for (adjacent <- adjacents; d = distanceArray(adjacent.y)(adjacent.x) if d != GameGrid.EmptySpaceNumber) yield d

      if (next != playerLocation) {
        val newDistance = distances.min + 1
        distanceArray(next.y)(next.x) = newDistance
      }

      val cellsToExplore = for (adjacentCell <- adjacents
                                if !coordinatesToExplore.contains(adjacentCell)
                                if adjacentCell.isWithin(width, height) &&
                                  adjacentCell != playerLocation &&
                                  array(adjacentCell.y)(adjacentCell.x) == GameGrid.EmptySpaceNumber &&
                                  distanceArray(adjacentCell.y)(adjacentCell.x) == GameGrid.EmptySpaceNumber
      ) yield adjacentCell

      for (cell <- cellsToExplore) {
        coordinatesToExplore.append(cell)
      }

    }
    distanceArray
  }

  def getNumberOfReachableCells(playerLocation: Coordinate): Int = {
    val distanceGrid = getDistanceGridForPlayer(playerLocation)

    var counter = 0
    // TODO Make less imperative
    for(array <- distanceGrid) {
      for (cell <- array) {
        if (cell != GameGrid.EmptySpaceNumber) {
          counter += 1
        }
      }
    }
    counter
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

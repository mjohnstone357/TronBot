import scala.io.Source
import scala.collection.mutable.Map

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
      val availableMoves: Set[Move] = gameGrid.getAvailableMoves(myPlayerNumber)

      // Impure output stuff

      if (availableMoves.isEmpty) {
        debug("Got trapped :(")
      } else {
        writeMove(availableMoves.head)
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
class GameGrid(width: Int, height: Int, array: Array[Array[Int]]) {


  private val playerLocations: Map[Int, Coordinate] = Map[Int, Coordinate]()

  def this(width: Int, height: Int) = {
    this(width, height, Array.fill(width, height)(GameGrid.EmptySpaceNumber))
  }

  def this(s: String) = {
    this(GameGrid.getDimensions(s)._1, GameGrid.getDimensions(s)._2, GameGrid.arrayFromParsing(s))

  }

  /**
   * Update the grid using the player information yielded for the latest turn.
   * @param playerInformation information indicating the location / announcing the death of each player
   */
  def update(playerInformation: List[PlayerInfo]) {
    for(info <- playerInformation) {
      info match {
        case PlayerLocation(playerNumber, Coordinate(x,y), _) =>
          playerLocations(playerNumber) = Coordinate(x,y)
          array(x)(y) = playerNumber
        case PlayerDead(playerNumber) => purgePlayer(playerNumber)
      }
    }
  }

  def purgePlayer(playerNumber: Int): Unit = {
    for(i <- 0 until width; j <- 0 until height) {
      val cell: Int = array(i)(j)
      if (cell == playerNumber) {
        array(i)(j) = GameGrid.EmptySpaceNumber
      }
    }
  }

  def rendered: String = {

    def padInt(x: Int) = {
      val string: String = x.toString
      if (string.length == 1) " " + string else string
    }
    val paddedCells: Array[Array[String]] = for (row <- array) yield for (cell <- row.reverse) yield padInt(cell)
    
    (for (row <- paddedCells) yield row.mkString(" ")).reverse.mkString("\n")
  }

  def getAvailableMoves(playerNumber: Int): Set[Move] = {

    val playerLocation = playerLocations(playerNumber)

    def cellIsValid(coordinate: Coordinate): Boolean = {
      val (x, y) = (coordinate.x, coordinate.y)
      x < width && x >= 0 &&
      y < height && y >= 0 &&
      array(x)(y) == GameGrid.EmptySpaceNumber
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

  def makeRow(rowString: String): Array[Int] = {
    val cells: Array[String] = for (cell <- rowString.split(" ") if !cell.isEmpty) yield cell
    for (cell <- cells.reverse) yield cell.toInt
  }

  def getDimensions(s: String): (Int, Int) = {
    val lines: List[String] = s.lines.toList
    val firstLine: String = lines.head
    val row: Array[String] = for (cell <- firstLine.split(" ") if !cell.isEmpty) yield cell

    val h = lines.size
    val w = row.size
    (w, h)
  }

  def arrayFromParsing(s: String) = {
    (for (line <- s.lines.toList) yield makeRow(line)).reverse.toArray
  }

}

class DistanceFinder(arr: Array[Array[Int]]) {
  private val array = arr.clone()
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

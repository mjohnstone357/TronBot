import org.scalatest._
import scala.io.Source

class PlayerTests extends FlatSpec with Matchers {

  "The PlayerInfo parse method" should "correctly parse a line containing four input values" in {

    val inputLine = "1 2 3 4"
    val info = PlayerInfo.parse(inputLine, 7)

    info should be(PlayerLocation(playerNumber = 7, head = Coordinate(3, 4), tail = Coordinate(1, 2)))
  }

  "The readOneTurn method" should "read from the given source and return a list of lines corresponding to a single turn" in {

    val source = Source.fromString(
      """2 0
        |blah
        |blah
        |other stuff
        |more stuff
      """.stripMargin)

    val turn: List[String] = Player.readOneTurn(source)

    turn should be(List("2 0", "blah", "blah"))
  }

  "The getPlayerInfos method" should "read a turn's input lines and yield PlayerInfos containing all " +
    "of the information that can be learned from those lines" in {
    val input =
      """2 0
        |3 5 3 4
        |-1 -1 -1 -1""".stripMargin

    val inputLines: List[String] = input.lines.toList
    inputLines.size should be(3)

    val output: List[PlayerInfo] = PlayerInfo.getPlayerInformation(inputLines)

    output should be(List(
      PlayerLocation(0, head = Coordinate(3, 4), tail = Coordinate(3, 5)),
      PlayerDead(1)
    ))
  }

  "The getAvailableMoves method" should "return no moves when the game grid has only one cell, " +
    "with the player occupying it" in {
    val grid = new GameGrid(1, 1)
    val playerNumber = 0

    grid.update(List(PlayerLocation(playerNumber, Coordinate(0,0), Coordinate(0,0))))

    val availableMoves: Set[Move] = grid.getAvailableMoves(0)
    availableMoves should be (Set())

  }

  /*
    0 1 2
   0    X
   1
   2
  */
  "The agent" should "consider only the directly (not diagonally) adjacent cells, and not the walls, " +
    "when the game grid is 3x3 and it is in the top left corner" in {
    val grid = new GameGrid(3, 3)
    val playerNumber = 0

    grid.update(List(PlayerLocation(playerNumber, Coordinate(2,0), Coordinate(2,0))))

    val availableMoves: Set[Move] = grid.getAvailableMoves(0)
    availableMoves should be (Set(Left(), Down()))

  }

  /*
    0 1 2
   0
   1  X
   2
  */
  it should "consider moving up, down, left or right when the game grid is 3x3 and the player is in the middle" in {
    val grid = new GameGrid(3, 3)
    val playerNumber = 0

    grid.update(List(PlayerLocation(playerNumber, Coordinate(1,1), Coordinate(1,1))))

    val availableMoves: Set[Move] = grid.getAvailableMoves(0)
    availableMoves should be (Set(Up(), Down(), Left(), Right()))

  }

//  /*
//    0 1 2
//   0
//   1  X
//   2
//  */
//  it should "return all four moves when the game grid is 3x3 and the player is in the middle" in {
//    val grid = new GameGrid(3, 3)
//    val playerNumber = 0
//
//    grid.update(List(PlayerLocation(playerNumber, Coordinate(1,1), Coordinate(1,1))))
//
//    val availableMoves: Set[Move] = grid.getAvailableMoves(0)
//    availableMoves should be (Set(Up(), Down(), Left(), Right()))
//
//  }

  "The game grid" should "render correctly when it is 3x3" in {
    val grid = new GameGrid(3, 3)

    val expected: String = """-1 -1 -1
                             |-1 -1 -1
                             |-1 -1 -1""".stripMargin
    grid.rendered should be (expected)
  }

  it should "render correctly when it is 3x3 and includes a single-digit number" in {
    val grid = new GameGrid(3, 3)

    grid.update(List(
      PlayerLocation(0, Coordinate(1, 1), Coordinate(1,1)),
      PlayerLocation(1, Coordinate(2, 0), Coordinate(2,0))))

    val expected: String = """-1 -1  1
                             |-1  0 -1
                             |-1 -1 -1""".stripMargin
    grid.rendered should be (expected)
  }

  it should "purge a player when requested" in {
    val grid = new GameGrid(3, 3)

    grid.update(List(PlayerLocation(2, Coordinate(0, 0), Coordinate(0, 0))))
    grid.update(List(PlayerLocation(2, Coordinate(1, 1), Coordinate(1, 1))))
    grid.update(List(PlayerLocation(2, Coordinate(2, 2), Coordinate(2, 2))))

    val initialGrid = """ 2 -1 -1
                        |-1  2 -1
                        |-1 -1  2""".stripMargin
    grid.rendered should be (initialGrid)

    grid.purgePlayer(2)

    val resultantGrid = """-1 -1 -1
                          |-1 -1 -1
                          |-1 -1 -1""".stripMargin

    grid.rendered should be (resultantGrid)
  }

  it should "parse a grid, then render the same string as was passed in" in {
    val inputGrid =
      """ 1  2  3
        | 4  5  6
        | 7  8  9""".stripMargin

    val grid = new GameGrid(inputGrid)

    grid.rendered should be (inputGrid)
  }

}



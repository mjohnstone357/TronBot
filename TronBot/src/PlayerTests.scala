import org.scalatest._
import scala.io.Source
import ArrayUtils._

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

  it should "parse a grid, creating a structure that can be indexed into using array(x)(y)" in {
    import ArrayUtils._

    val inputGrid =
      """ 1  2  3
        | 4  5  6
        | 7  8  9""".stripMargin

    val array: Array[Array[Int]] = arrayFromParsing(inputGrid)

    get(array, 0, 0) should be (1)
    get(array, 1, 0) should be (2)
    get(array, 2, 0) should be (3)
    get(array, 0, 1) should be (4)
    get(array, 1, 1) should be (5)
    get(array, 2, 1) should be (6)
    get(array, 0, 2) should be (7)
    get(array, 1, 2) should be (8)
    get(array, 2, 2) should be (9)
  }

  it should "parse a grid, then render the same string as was passed in" in {
    val inputGrid =
      """ 1  2  3
        | 4  5  6
        | 7  8  9""".stripMargin

    val grid = new GameGrid(inputGrid)

    grid.rendered should be (inputGrid)
  }

  "The DistanceFinder" should "compute the distance to each point on an empty input grid, starting from the middle" in {
    val inputGrid =
      """-1 -1 -1
        |-1 -1 -1
        |-1 -1 -1""".stripMargin

    val outputGrid =
      """ 2  1  2
        | 1  0  1
        | 2  1  2""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(1,1))

    ArrayUtils.render(distanceGrid) should be (outputGrid)

  }

  it should "compute the distance to each point on a larger input grid, starting from the left side" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1""".stripMargin

    val outputGrid =
      """ 2  3  4  5  6
        | 1  2  3  4  5
        | 0  1  2  3  4
        | 1  2  3  4  5
        | 2  3  4  5  6""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(0,2))

    ArrayUtils.render(distanceGrid) should be (outputGrid)

  }

  it should "work with non-square grids" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1""".stripMargin

    val outputGrid =
      """ 1  2  3  4  5
        | 0  1  2  3  4
        | 1  2  3  4  5""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(0,1))

    ArrayUtils.render(distanceGrid) should be (outputGrid)

  }

  it should "find distances around obstacles" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1  1 -1 -1
        |-1 -1  2 -1 -1
        |-1 -1  3 -1 -1""".stripMargin

    val outputGrid =
      """ 2  3  4  5  6
        | 1  2  3  4  5
        | 0  1 -1  5  6
        | 1  2 -1  6  7
        | 2  3 -1  7  8""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(0,2))

    ArrayUtils.render(distanceGrid) should be (outputGrid)
  }

  it should "find distances around obstacles where there is a single-cell bottleneck" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1 -1  0 -1 -1
        |-1 -1  1 -1 -1
        |-1 -1  2 -1 -1
        |-1 -1  3 -1 -1""".stripMargin

    val outputGrid =
      """ 2  3  4  5  6
        | 1  2 -1  6  7
        | 0  1 -1  7  8
        | 1  2 -1  8  9
        | 2  3 -1  9 10""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(0,2))

    ArrayUtils.render(distanceGrid) should be (outputGrid)
  }

  it should "find distances around obstacles where are two paths and one is shorter" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1 -1  0 -1 -1
        |-1 -1  1 -1 -1
        |-1 -1  2 -1 -1
        |-1 -1 -1 -1 -1""".stripMargin

    val outputGrid =
      """ 2  3  4  5  6
        | 1  2 -1  6  7
        | 0  1 -1  7  8
        | 1  2 -1  6  7
        | 2  3  4  5  6""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val distanceFinder: DistanceFinder = new DistanceFinder(grid)
    val distanceGrid: Array[Array[Int]] = distanceFinder.getDistanceGridForPlayer(Coordinate(0,2))

    ArrayUtils.render(distanceGrid) should be (outputGrid)
  }

  "The move analyser" should "return a map including all four moves when the player is in the middle of the grid" in {
    val inputGrid = """-1 -1 -1
                      |-1  0 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
      playerArray = grid,
      playerLocationMap = Map(0 -> Coordinate(1, 1)),
      currentPlayer = 0)

    goodnessMap.keySet should be (Move.AllMoves)
  }

  it should "return a map including the two legal moves when the player is in a corner of the grid" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
      playerArray = grid,
      playerLocationMap = Map(0 -> Coordinate(0, 0)),
      currentPlayer = 0)

    goodnessMap.keySet should be (Set(Right(), Down()))
  }

  it should "find a move which immediately traps the player to be less desirable than one which does not" in {
    val inputGrid = """ 0 -1 -1
                      |-1  3 -1
                      | 3  3 -1""".stripMargin

    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
      playerArray = arrayFromParsing(inputGrid),
      playerLocationMap = Map(0 -> Coordinate(0, 0)),
      currentPlayer = 0)

    goodnessMap.keySet should be (Set(Right(), Down()))
    val downGoodness: Int = goodnessMap(Down())
    val rightGoodness: Int = goodnessMap(Right())

    downGoodness should be < rightGoodness
  }

  it should "find a move which traps an opponent to be more desirable than one which does not" in {
    val inputGrid = """ 0 -1 -1
                      | 0  0 -1
                      | 3 -1 -1""".stripMargin

    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
      playerArray = arrayFromParsing(inputGrid),
      playerLocationMap = Map(0 -> Coordinate(1, 1), 3 -> Coordinate(0, 2)),
      currentPlayer = 0)

    goodnessMap.keySet should be (Set(Up(), Right(), Down()))

    goodnessMap(Down()) should be > goodnessMap(Up())
    goodnessMap(Down()) should be > goodnessMap(Right())
  }

  // TODO Fix this test by implementing 2-move lookahead
//  it should "find a move which cuts off a section of grid on the following move to be less desirable than one that does not" in {
//    val inputGrid =
//      """-1 -1 -1  0 -1
//        |-1 -1 -1  0 -1
//        |-1 -1 -1  0 -1
//        |-1 -1 -1  0 -1
//        |-1 -1 -1 -1 -1""".stripMargin
//
//    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
//      playerArray = arrayFromParsing(inputGrid),
//      playerLocationMap = Map(0 -> Coordinate(3, 3)),
//      currentPlayer = 0)
//
//    goodnessMap.keySet should be (Set(Left(), Right(), Down()))
//    val downGoodness: Int = goodnessMap(Down())
//    val leftGoodness: Int = goodnessMap(Left())
//
//    downGoodness should be < leftGoodness
//  }

  it should "prefer a move which increases the player's ability to reach cells before its opponent" in {
    val inputGrid =
      """-1 -1 -1 -1 -1
        |-1  0 -1 -1 -1
        |-1 -1 -1 -1 -1
        |-1 -1 -1  2 -1
        |-1 -1 -1 -1 -1""".stripMargin

    val goodnessMap: Map[Move, Int] = MoveAnalyser.determineMoveGoodness(
      playerArray = arrayFromParsing(inputGrid),
      playerLocationMap = Map(0 -> Coordinate(1, 1), 2 -> Coordinate(3, 3)),
      currentPlayer = 0)

    goodnessMap.keySet should be (Set(Up(), Down(), Left(), Right()))

    // Right and Down are both preferable to Up and Left
    List(goodnessMap(Up()), goodnessMap(Left())).max should be < List(goodnessMap(Right()), goodnessMap(Down())).min

  }

  "The grid racer" should "indicate the player which can first reach each cell in an almost empty 3x3 grid" in {
    val inputGrid =
      """ 0 -1 -1
        |-1 -1 -1
        |-1 -1  2""".stripMargin
    
    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val raceGrid: Array[Array[Int]] = GridRacer.makeRaceGrid(grid, Map(0 -> Coordinate(0, 0), 2 -> Coordinate(2, 2)))

    val outputGrid =
      """ 0  0 -1
        | 0 -1  2
        |-1  2  2""".stripMargin

    ArrayUtils.render(raceGrid) should be (outputGrid)

  }

  it should "indicate that each player can reach three cells before the other" in {
    val inputGrid =
      """ 0 -1 -1
        |-1 -1 -1
        |-1 -1  2""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val scores: Map[Int, Int] = GridRacer.getPlayerScores(grid, Map(0 -> Coordinate(0, 0), 2 -> Coordinate(2, 2)))

    scores should be (Map(0 -> 3, 2 -> 3))
  }

  it should "not have silly bug" in {
    val inputGrid = """ 0 -1 -1
                      | 0  0 -1
                      | 3  0 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val raceGrid: Array[Array[Int]] = GridRacer.makeRaceGrid(grid, Map(0 -> Coordinate(1, 2), 3 -> Coordinate(0, 2)))

    val outputGrid =
      """-1  0  0
        |-1 -1  0
        | 3  0  0""".stripMargin

    ArrayUtils.render(raceGrid) should be (outputGrid)
  }

  it should "indicate that player 0 can reach 4 cells first, as player 3 is trapped" in {
    val inputGrid = """ 0 -1 -1
                      | 0  0 -1
                      | 3  0 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val scores: Map[Int, Int] = GridRacer.getPlayerScores(grid, Map(0 -> Coordinate(1, 2), 3 -> Coordinate(0, 2)))

    scores should be (Map(0 -> 5, 3 -> 1))
  }

  "The move sequencer" should "return the starting position when applying an empty list of moves" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(grid, Coordinate(0,0), List.empty)

    outcome match {
      case EndedUpAtLocation(Coordinate(0,0), _) => ()
      case _ => Failed
    }
  }

  it should "indicate an illegal move if the move sequence would take the player off the grid" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Up()))

    outcome should be (PerformedIllegalMove())
  }

  it should "indicate an illegal move if a longer move sequence would take the player off the grid" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Right(), Down(), Right(), Down(), Down()))

    outcome should be (PerformedIllegalMove())
  }

  it should "indicate a successful move to the opposite corner of the grid if there are no obstacles" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Right(), Down(), Right(), Down()))

    outcome match {
      case EndedUpAtLocation(Coordinate(2,2), _) => ()
      case _ => fail()
    }
  }

  it should "indicate an illegal move if the sequence moves to the opposite corner but there is an obstacle" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1  2
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Right(), Down(), Right(), Down()))

    outcome should be (PerformedIllegalMove())
  }

  it should "indicate an illegal move if the sequence makes the player trip over its own tail" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Right(), Down(), Right(), Up(), Left()))

    outcome should be (PerformedIllegalMove())
  }

  it should "indicate an illegal move if the sequence makes the player trip over its own tail then carries on into an empty space" in {
    val inputGrid = """ 0 -1 -1
                      |-1 -1 -1
                      |-1 -1 -1""".stripMargin

    val grid: Array[Array[Int]] = arrayFromParsing(inputGrid)

    val outcome: MoveSequenceOutcome = MoveSequencer.computeSequenceResult(
      playerArray = grid,
      startingLocation = Coordinate(0,0),
      moveSequence = List(Right(), Down(), Right(), Up(), Left(), Left(), Down()))

    outcome should be (PerformedIllegalMove())
  }



}

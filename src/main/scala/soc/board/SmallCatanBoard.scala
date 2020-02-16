package soc.board

import soc.core.Roll
import soc.inventory.{Brick, Misc, Ore, Port, Resource, Sheep, Wheat, Wood}

case class SmallBoardConfiguration(hexes: List[Hex], ports: List[Port]) extends SimpleCatanBoardConfiguration
object SmallCatanBoard extends SimpleBoardLayout[SmallBoardConfiguration] with BoardMapping[SmallBoardConfiguration] {

  type SmallBoard = CatanBoard[SmallBoardConfiguration]

  override def apply(hexes: List[Hex], ports: List[Port]): SmallBoardConfiguration = SmallBoardConfiguration(hexes, ports)

  val vertexMap: Map[Int, List[Int]] = Map (
    0 -> List(0, 1, 2, 27, 26, 25),
    1 -> List(2, 3, 4, 29, 28, 27),
    2 -> List(4, 5, 6, 31, 30, 29),
    3 -> List(6, 7, 8, 9, 32, 31),
    4 -> List(32, 9, 10, 11, 12, 33),
    5 -> List(34, 33, 12, 13, 14, 15),
    6 -> List(36, 35, 34, 15, 16, 17),
    7 -> List(38, 37, 36, 17, 18, 19),
    8 -> List(22, 39, 38, 19, 20, 21),
    9 -> List(24, 25, 26, 39, 22, 23),
    10 -> List(26, 27, 28, 37, 38, 39),
    11 -> List(28, 29, 30, 35, 36, 37)
  )

  lazy val portEdges = Seq((0, 1), (3, 4), (7, 8), (10, 11), (13, 14), (16, 17), (20, 21), (23, 24))

  lazy val hexMapping = Map (
    0 -> "C3", 1 -> "D4", 2 -> "E5", 3 -> "F6", 4 -> "F8", 5 -> "E9", 6 -> "D8", 7 -> "C7", 8 -> "B6", 9 -> "B4",
    10 -> "C5", 11 -> "D6", 12 -> "E7"
  )

  lazy val vertexMapping = Map(
    0 -> "B2R", 1 -> "D2L", 2 -> "C3R", 3 -> "E3L", 4 -> "D4R", 5 -> "F4L", 6 -> "E5R", 7 -> "G5L", 8 -> "F6R", 9 -> "G7L",
    10 -> "F8R", 11 -> "G9L", 12 -> "E9R", 13 -> "F10L", 14 -> "D10R", 15 -> "E9L", 16 -> "C9R", 17 -> "D8L", 18 -> "B8R",
    19 -> "C7L", 20 -> "A7R", 21 -> "B6L", 22 -> "A5R", 23 -> "B4L", 24 -> "A3R", 25 -> "C3L", 26 -> "B4R", 27 -> "D4L",
    28 -> "C5R", 29 -> "E5L", 30 -> "D6R", 31 -> "F6L", 32 -> "E7R", 33 -> "F8L", 34 -> "D8R", 35 -> "E7L", 36 -> "C7R",
    37 -> "D6L", 38 -> "B6R", 39 -> "C5L"
  )

  val resourceCounts: Map[Resource, Int] = Map(
    Wood -> 3,
    Sheep -> 3,
    Wheat -> 3,
    Brick -> 2,
    Ore -> 2
  )

  val portCounts: Map[Port, Int] = Map(
    Wood -> 1,
    Sheep -> 1,
    Wheat -> 1,
    Brick -> 1,
    Ore -> 1,
    Misc -> 3
  )

  val validRolls: Seq[Roll] = Seq(2, 3, 3, 4, 4, 6, 6, 7, 7, 8).map(Roll(_))
  override val cannotNeighbor: Seq[Roll] = Seq(4, 6).map(Roll(_))

  override val robberRoll: Int = 5
  override val diceProbability: Int = 16
  override val numDesert: Int = 0

}

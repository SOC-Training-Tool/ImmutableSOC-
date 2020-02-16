package soc.board

import soc.core.Roll
import soc.inventory._

import scala.language.implicitConversions

case class BaseBoardConfiguration(hexes: List[Hex], ports: List[Port]) extends SimpleCatanBoardConfiguration

object BaseCatanBoard extends SimpleBoardLayout[BaseBoardConfiguration] with BoardMapping[BaseBoardConfiguration] {

  type BaseBoard = CatanBoard[BaseBoardConfiguration]

  override def apply(hexes: List[Hex], ports: List[Port]): BaseBoardConfiguration = BaseBoardConfiguration(hexes, ports)

  val vertexMap: Map[Int, List[Int]] = Map (
    0 -> List(0, 1, 2, 31, 30, 29),
    1 -> List(2, 3, 4, 33, 32, 31),
    2 -> List(4, 5, 6, 7, 34, 33),
    3 -> List(34, 7, 8, 9, 36, 35),
    4 -> List(36, 9, 10, 11, 12, 37),
    5 -> List(38, 37, 12, 13, 14, 39),
    6 -> List(40, 39, 14, 15, 16, 17),
    7 -> List(42, 41, 40, 17, 18, 19),
    8 -> List(22, 43, 42, 19, 20, 21),
    9 -> List(24, 45, 44, 43, 22, 23),
    10 -> List(26, 27, 46, 45, 24, 25),
    11 -> List(27, 28, 29, 30, 47, 46),
    12 -> List(30, 31, 32, 48, 53, 47),
    13 -> List(32, 33, 34, 35, 49, 48),
    14 -> List(49, 35, 36, 37, 38, 50),
    15 -> List(51, 50, 38, 39, 40, 41),
    16 -> List(44, 52, 51, 41, 42, 43),
    17 -> List(46, 47, 53, 52, 44, 45),
    18 -> List(53, 48, 49, 50, 51, 52)
  )

  lazy val portEdges = Seq((0, 1), (3, 4), (7, 8), (10, 11), (13, 14), (17, 18), (20, 21), (23, 24), (27, 28))

  lazy val hexMapping = Map (
    0 -> "D2", 1 -> "E3", 2 -> "F4", 3 -> "F6", 4 -> "F8", 5 -> "E9", 6 -> "D10", 7 -> "C9", 8 -> "B8", 9 -> "B6",
    10 -> "B4", 11 -> "C3", 12 -> "D4", 13 -> "E5", 14 -> "E7", 15 -> "D8", 16 -> "C7", 17 -> "C5", 18 -> "D6",
  )

  lazy val vertexMapping = Map(
    0 -> "C1R", 1 -> "E1L", 2 -> "D2R", 3 -> "F2L", 4 -> "E3R", 5 -> "G3L", 6 -> "F4R", 7 -> "G5L", 8 -> "F6R", 9 -> "G7L",
    10 -> "F8R", 11 -> "G9L", 12 -> "E9R", 13 -> "F10L", 14 -> "D10R", 15 -> "E11L", 16 -> "C11R", 17 -> "D10L", 18 -> "B10R",
    19 -> "C9L", 20 -> "A9R", 21 -> "B8L", 22 -> "A7R", 23 -> "B6L", 24 -> "A5R", 25 -> "B4L", 26 -> "A3R", 27 -> "C3L",
    28 -> "B2R", 29 -> "D2L", 30 -> "C3R", 31 -> "E3L", 32 -> "D4R", 33 -> "F4L", 34 -> "E5R", 35 -> "F6L", 36 -> "E7R",
    37 -> "F8L", 38 -> "D8R", 39 -> "E9L", 40 -> "C9R", 41 -> "D8L", 42 -> "B8R", 43 -> "C7L", 44 -> "B6R", 45 -> "C5L",
    46 -> "B4R", 47 -> "D4L", 48 -> "C5R", 49 -> "E5L", 50 -> "D6R", 51 -> "E7L", 52 -> "C7R", 53 -> "D6L"
  )

  val resourceCounts: Map[Resource, Int] = Map(
    Wood -> 4,
    Sheep -> 4,
    Wheat -> 4,
    Brick -> 3,
    Ore -> 3
  )

  val portCounts: Map[Port, Int] = Map(
    Wood -> 1,
    Sheep -> 1,
    Wheat -> 1,
    Brick -> 1,
    Ore -> 1,
    Misc -> 4
  )

  val validRolls: Seq[Roll] = Seq(2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12).map(Roll(_))
  override val cannotNeighbor: Seq[Roll] = Seq(6, 8).map(Roll(_))

  override val robberRoll: Int = 7
  override val diceProbability: Int = 36
  override val numDesert: Int = 1

}
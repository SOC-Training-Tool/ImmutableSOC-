package board

import core.Roll
import inventory._

import scala.annotation.tailrec
import scala.util.Random
import scala.language.implicitConversions

case class BaseBoardConfiguration(hexes: List[Hex], ports: List[Port]) extends BoardConfiguration

object BaseCatanBoard extends BoardGenerator[BaseBoardConfiguration] {

  implicit val generator: BoardGenerator[BaseBoardConfiguration] = this

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

  def portMapFunc(ports: List[Port]): Map[Int, Port] = Map(
    0 -> ports(0), 1 -> ports(0),
    3 -> ports(1), 4 -> ports(1),
    7 -> ports(2), 8 -> ports(2),
    10 -> ports(3), 11 -> ports(3),
    13 -> ports(4), 14 -> ports(4),
    17 -> ports(5), 18 -> ports(5),
    20 -> ports(6), 21 -> ports(6),
    23 -> ports(7), 24 -> ports(7),
    27 -> ports(8), 28 -> ports(8)
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

  val validRolls: Seq[Roll] = Seq(2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12).map(Roll)

  def apply(hexes: List[Hex], ports: List[Port]): CatanBoard = {

    require(hexes.length == 19, "There should be 19 hexes")
    require(ports.length == 9, "There should be 9 ports")

    require(hexes.filter {
      case Desert => false
      case _ => true
    }.groupBy(_.getResource.get).forall { case (resource, list) =>
        list.length == resourceCounts(resource)
    }, "There are not enough hexes of each resource type")

    CatanBoard(vertexMap, portMapFunc, hexes, ports)
  }

  @tailrec
  override def randomBoard(implicit rand: Random): BaseBoardConfiguration = {
    val resources = resourceCounts.toSeq.flatMap {
      case (resource, amt) => (1 to amt).map(_ => resource)
    }

    val hexes = rand.shuffle {
      Desert ::
      (rand.shuffle(resources) zip rand.shuffle(validRolls)).map {
        case (resource, roll) => ResourceHex(resource, roll)
      }.toList
    }
    val ports = rand.shuffle {
      portCounts.toSeq.flatMap {
        case (port: Port, amt) => (1 to amt).map(_ => port)
      }.toList
    }

    val config = BaseBoardConfiguration(hexes, ports)
    if (CatanBoard.checkValid(this.apply(config))) config
    else randomBoard
  }

  implicit override def apply(config: BaseBoardConfiguration): CatanBoard = apply(config.hexes, config.ports)
}
package soc

import soc.board._
import soc.core.Roll
import soc.inventory._

object CatanFixtures {

  val singleHexBoard = {
    val vertexMap = Map(0 -> List(0, 1, 2, 3, 4, 5)).view.mapValues(_.map(Vertex)).toMap
    val portMap = Map(Edge(Vertex(0), Vertex(1)) -> Misc)
    val hexes = List(ResourceHex(Wood, Roll(6)))
    CatanBoard(vertexMap, portMap, hexes)
  }

  val baseBoardConfig = {
    val hexes = List(
      ResourceHex(Wheat, Roll(6)),
      ResourceHex(Ore, Roll(2)),
      ResourceHex(Sheep, Roll(5)),
      ResourceHex(Ore, Roll(8)),
      ResourceHex(Wood, Roll(4)),
      ResourceHex(Brick, Roll(11)),
      ResourceHex(Sheep, Roll(12)),
      ResourceHex(Ore, Roll(9)),
      ResourceHex(Sheep, Roll(10)),
      ResourceHex(Brick, Roll(8)),
      Desert,
      ResourceHex(Wheat, Roll(3)),
      ResourceHex(Sheep, Roll(9)),
      ResourceHex(Brick, Roll(10)),
      ResourceHex(Wood, Roll(3)),
      ResourceHex(Wood, Roll(6)),
      ResourceHex(Wheat, Roll(5)),
      ResourceHex(Wood, Roll(4)),
      ResourceHex(Wheat, Roll(11))
    )

    val ports: List[Port] = List(Misc, Ore, Misc, Wheat, Misc, Brick, Wood, Sheep, Misc)
    BaseBoardConfiguration(hexes, ports)
  }

  val baseBoard = BaseCatanBoard(baseBoardConfig)

}

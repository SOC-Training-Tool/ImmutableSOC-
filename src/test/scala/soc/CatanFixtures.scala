package soc

import board._
import core.Roll
import inventory._
import inventory.resources.ResourceSet
import moves._

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

  // with baseBoard
  val testMoveResults: List[MoveResult] = List (
    InitialPlacementMove(true, Vertex(41), Edge(Vertex(40), Vertex(41))),
    InitialPlacementMove(true, Vertex(34), Edge(Vertex(7), Vertex(34))),
    InitialPlacementMove(true, Vertex(44), Edge(Vertex(44), Vertex(45))),
    InitialPlacementMove(true, Vertex(36), Edge(Vertex(9), Vertex(36))),
    InitialPlacementMove(false, Vertex(31), Edge(Vertex(2), Vertex(31))),
    InitialPlacementMove(false, Vertex(47), Edge(Vertex(30), Vertex(47))),
    InitialPlacementMove(false, Vertex(48), Edge(Vertex(48), Vertex(49))),
    InitialPlacementMove(false, Vertex(22), Edge(Vertex(21), Vertex(22))),
    // 0
    RollResult(Roll(5)),
    EndTurnMove,
    // 1
    RollResult(Roll(6)),
    EndTurnMove,
    // 2
    RollResult(Roll(4)),
    EndTurnMove,
    // 3
    RollResult(Roll(4)),
    BuyDevelopmentCardResult(Seq(3), Some(Knight)),
    EndTurnMove,
    // 0
    RollResult(Roll(9)),
    BuildRoadMove(Edge(Vertex(17), Vertex(40))),
    EndTurnMove,
    // 1
    RollResult(Roll(8)),
    BuyDevelopmentCardResult(Seq(1), Some(Knight)),
    EndTurnMove,
    // 2
    RollResult(Roll(9)),
    PortTradeMove(ResourceSet(wo = 4), ResourceSet(or = 1)),
    BuyDevelopmentCardResult(Seq(2), Some(CatanPoint)),
    BuildRoadMove(Edge(Vertex(24), Vertex(45))),
    EndTurnMove,
    // 3
    RollResult(Roll(7)),
    MoveRobberAndStealResult(Seq(0, 3), 9, Some(RobPlayer(0, Some(Brick)))),
    BuyDevelopmentCardResult(Seq(3), Some(CatanPoint)),
    BuildRoadMove(Edge(Vertex(1), Vertex(2))),
    EndTurnMove,
    // 0
    RollResult(Roll(6)),
    EndTurnMove,
    // 1
    RollResult(Roll(5)),
    PortTradeMove(ResourceSet(sh = 4), ResourceSet(wo = 1)),
    BuildRoadMove(Edge(Vertex(49), Vertex(50))) ,
    EndTurnMove,
    // 2
    RollResult(Roll(3)),
    EndTurnMove,
    // 3
    RollResult(Roll(10)),
    KnightResult(MoveRobberAndStealResult(Seq(1, 3), 13, Some(RobPlayer(1, Some(Brick))))),
    build.BuildSettlementMove(Vertex(1)),
    EndTurnMove,
    // 0
    RollResult(Roll(10)),
    BuyDevelopmentCardResult(Seq(0), Some(Knight)),
    EndTurnMove,
    // 1
    KnightResult(MoveRobberAndStealResult(Seq(1, 3), 0, Some(RobPlayer(3, Some(Wood))))),
    RollResult(Roll(8)),
    EndTurnMove,
    // 2
    RollResult(Roll(9)),
    EndTurnMove,
    // 3
    RollResult(Roll(7)),
    MoveRobberAndStealResult(Seq(0, 3), 9, Some(RobPlayer(0, Some(Sheep)))),
    EndTurnMove,
    // 0
    KnightResult(MoveRobberAndStealResult(Seq(0, 3), 3, Some(RobPlayer(3, Some(Brick))))),
    RollResult(Roll(5)),
    EndTurnMove,
    // 1
    RollResult(Roll(8)),
    EndTurnMove,
    // 2
    RollResult(Roll(10)),
    EndTurnMove,
    // 3
    RollResult(Roll(11)),
    EndTurnMove,
    // 0
    RollResult(Roll(10)),
    build.BuildSettlementMove(Vertex(17)),
    EndTurnMove,
    // 1
    RollResult(Roll(11)),
    build.BuildSettlementMove(Vertex(50)),
    BuyDevelopmentCardResult(Seq(1), Some(Knight)),
    EndTurnMove,
    // 2
    RollResult(Roll(5)),
    PortTradeMove(ResourceSet(wh = 4), ResourceSet(wo = 1)),
    build.BuildSettlementMove(Vertex(24)),
    PortTradeMove(ResourceSet(sh = 2), ResourceSet(wo = 1)),
    BuildRoadMove(Edge(Vertex(29), Vertex(30))),
    EndTurnMove,
    // 3
    RollResult(Roll(3)),
    EndTurnMove,
    // 0
    RollResult(Roll(10)),
    BuyDevelopmentCardResult(Seq(0), Some(RoadBuilder)),
    EndTurnMove,
    // 1
    KnightResult(MoveRobberAndStealResult(Seq(1, 2), 9, Some(RobPlayer(2, Some(Wheat))))),
    RollResult(Roll(9)),
    BuildRoadMove(Edge(Vertex(7), Vertex(8))),
    PortTradeMove(ResourceSet(br = 4), ResourceSet(wo = 1)),
    build.BuildSettlementMove(Vertex(8)),
    EndTurnMove,
    // 2
    RollResult(Roll(5)),
    EndTurnMove,
    // 3
    RollResult(Roll(8)),
    EndTurnMove,
    // 0
    RollResult(Roll(5)),
    BuildCityMove(Vertex(41)),
    EndTurnMove,
    // 1
    RollResult(Roll(5)),
    EndTurnMove,
    // 2
    RollResult(Roll(8)),
    EndTurnMove,
    // 3
    RollResult(Roll(11)),
    EndTurnMove,
    // 0
    RollResult(Roll(9)),
    BuildCityMove(Vertex(22)),
    EndTurnMove,
    // 1
    RollResult(Roll(5)),
    BuildCityMove(Vertex(34)),
    PortTradeMove(ResourceSet(sh = 3), ResourceSet(wh = 1)),
    BuyDevelopmentCardResult(Seq(1), Some(CatanPoint)),
    EndTurnMove,
    // 2
    RollResult(Roll(8)),
    EndTurnMove,
    // 3
    RollResult(Roll(8)),
    PortTradeMove(ResourceSet(sh = 3), ResourceSet(br = 1)),
    BuildRoadMove(Edge(Vertex(9), Vertex(10))),
    EndTurnMove,
    // 0
    RollResult(Roll(7)),
    DiscardResourcesResult(Map(1 -> ResourceSet(or = 3, sh = 1))),
    MoveRobberAndStealResult(Seq(0,1), 3, Some(RobPlayer(1, Some(Ore)))),
    BuyDevelopmentCardResult(Seq(0), Some(Knight)),
    EndTurnMove,
    // 1
    RollResult(Roll(5)),
    EndTurnMove,
    // 2
    RollResult(Roll(8)),
    PortTradeMove(ResourceSet(wh = 4), ResourceSet(wo = 1)),
    build.BuildSettlementMove(Vertex(29)),
    EndTurnMove,
    // 3
    RollResult(Roll(5)),
    PortTradeMove(ResourceSet(or = 3), ResourceSet(wh = 1)),
    BuyDevelopmentCardResult(Seq(3), Some(Monopoly)),
    EndTurnMove,
    // 0
    RollResult(Roll(10)),
    RoadBuilderMove(Edge(Vertex(20), Vertex(21)), Some(Edge(Vertex(39), Vertex(40)))),
    PortTradeMove(ResourceSet(br = 2), ResourceSet(wo = 1)),
    build.BuildSettlementMove(Vertex(20)),
    EndTurnMove,
    // 1
    RollResult(Roll(5)),
    PortTradeMove(ResourceSet(sh = 3), ResourceSet(wh = 1)),
    BuyDevelopmentCardResult(Seq(1), Some(YearOfPlenty)),
    PortTradeMove(ResourceSet(br = 3), ResourceSet(wh = 1)),
    BuyDevelopmentCardResult(Seq(1), Some(YearOfPlenty)),
    EndTurnMove,
    // 2
    RollResult(Roll(8)),
    EndTurnMove,
    // 3
    RollResult(Roll(6)),
    MonopolyResult(Map(0 -> ResourceSet(wh = 6), 2 -> ResourceSet(wh = 3))),
    PortTradeMove(ResourceSet(wh = 3), ResourceSet(or = 1)),
    PortTradeMove(ResourceSet(wh = 3), ResourceSet(or = 1)),
    PortTradeMove(ResourceSet(wh = 3), ResourceSet(or = 1)),
    BuildCityMove(Vertex(31)),
    EndTurnMove,
    // 0
    RollResult(Roll(3)),
    KnightResult(MoveRobberAndStealResult(Seq(0, 3), 0, Some(RobPlayer(3, Some(Wood))))),
    PortTradeMove(ResourceSet(wo = 2), ResourceSet(wh = 1)),
    build.BuildSettlementMove(Vertex(39)),
    EndTurnMove,
    // 1
    RollResult(Roll(10)),
    YearOfPlentyMove(Ore, Wheat),
    BuyDevelopmentCardResult(Seq(1), Some(Knight)),
    BuildRoadMove(Edge(Vertex(35), Vertex(49))),
    BuildRoadMove(Edge(Vertex(34), Vertex(35))),
    EndTurnMove,
    // 2
    RollResult(Roll(11)),
    EndTurnMove,
    // 3
    RollResult(Roll(6)),
    EndTurnMove,
    // 0
    RollResult(Roll(5)),
    BuildRoadMove(Edge(Vertex(19), Vertex(20))),
    BuildRoadMove(Edge(Vertex(19), Vertex(42))),
    EndTurnMove,
    // 1
    KnightResult(MoveRobberAndStealResult(Seq(0, 1), 7, Some(RobPlayer(0, Some(Wheat))))),
    RevealPoint,
    EndTurnMove,
    //2
    RevealPoint,
    EndTurnMove,
    //3
    RevealPoint,
    EndTurnMove,

  )

  def perspectiveMoveResults(perspectivePlayers: Seq[Int]): Map[Int, Seq[MoveResult]] = {
    val perspectiveMoves: Seq[Map[Int, MoveResult]] = testMoveResults.map(_.getPerspectiveResults(perspectivePlayers))
    perspectivePlayers.map { playerId =>
      playerId -> perspectiveMoves.map(_(playerId))
    }.toMap
  }

  lazy val noInfoPerspective: Seq[MoveResult] = perspectiveMoveResults(Seq(-1))(-1)
}

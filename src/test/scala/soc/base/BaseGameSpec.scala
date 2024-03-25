package soc.base

import game.InventorySet
import org.scalatest.{FunSpec, Matchers}
import shapeless.Coproduct
import soc.base.BaseGame.{DevelopmentCards, ImperfectInfoMoves, PerfectInfoMoves, PerfectInfoState, PublicInfoState}
import soc.base.actions.developmentcards._
import soc.core.Resources._
import soc.core._
import soc.core.state.PlayerPoints


class BaseGameSpec extends FunSpec with Matchers {

  val board: BaseBoard[Resource] = BaseBoard(
    List[Hex[Resource]](
      ResourceHex(WHEAT, 6),
      ResourceHex(ORE, 2),
      ResourceHex(SHEEP, 5),
      ResourceHex(ORE, 8),
      ResourceHex(WOOD, 4),
      ResourceHex(BRICK, 11),
      ResourceHex(SHEEP, 12),
      ResourceHex(ORE, 9),
      ResourceHex(SHEEP, 10),
      ResourceHex(BRICK, 8),
      Desert,
      ResourceHex(WHEAT, 3),
      ResourceHex(SHEEP, 9),
      ResourceHex(BRICK, 10),
      ResourceHex(WOOD, 3),
      ResourceHex(WOOD, 6),
      ResourceHex(WHEAT, 5),
      ResourceHex(WOOD, 4),
      ResourceHex(WHEAT, 11))
  )

  val initPerfectInfoState: PerfectInfoState =
    BaseGame.initGame[PerfectInfoState](board, InventorySet(Map(WOOD -> 19, BRICK -> 19, SHEEP -> 19, WHEAT -> 19, ORE -> 19)), 25)

  val initPublicInfoState: PublicInfoState =
    BaseGame.initGame[PublicInfoState](board, InventorySet(Map(WOOD -> 19, BRICK -> 19, SHEEP -> 19, WHEAT -> 19, ORE -> 19)), 25)


  val testMoveResults: List[PerfectInfoMoves] = List(
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(41), Edge(Vertex(40), Vertex(41)), true, 0)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(34), Edge(Vertex(7), Vertex(34)), true, 1)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(44), Edge(Vertex(44), Vertex(45)), true, 2)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(36), Edge(Vertex(9), Vertex(36)), true, 3)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(31), Edge(Vertex(2), Vertex(31)), false, 3)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(47), Edge(Vertex(30), Vertex(47)), false, 2)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(48), Edge(Vertex(48), Vertex(49)), false, 1)),
    Coproduct[PerfectInfoMoves](InitialPlacementMove(Vertex(22), Edge(Vertex(21), Vertex(22)), false, 0)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 5)),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 6)),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 4)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 4)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(3, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 9)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(0, Coproduct[DevelopmentCards](Point))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(0, Edge(Vertex(17), Vertex(40)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 8)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 9)),
    Coproduct[PerfectInfoMoves](PortTradeMove(2, ResourceSet(WOOD, WOOD, WOOD, WOOD), ResourceSet(ORE))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(2, Coproduct[DevelopmentCards](Point))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(2, Edge(Vertex(24), Vertex(45)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 7)),
    Coproduct[PerfectInfoMoves](PerfectInfoRobberMoveResult(3, 9, Some(PlayerSteal(3, BRICK)))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(3, Coproduct[DevelopmentCards](Point))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(3, Edge(Vertex(1), Vertex(2)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 6)),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 5)),
    Coproduct[PerfectInfoMoves](PortTradeMove(1, ResourceSet(SHEEP, SHEEP, SHEEP, SHEEP), ResourceSet(WOOD))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(1, Edge(Vertex(49), Vertex(50)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 3)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 10)),
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult(PerfectInfoRobberMoveResult(3, 13, Some(PlayerSteal(1, BRICK))))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(3, Vertex(1))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 10)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(0, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult(PerfectInfoRobberMoveResult(1, 0, Some(PlayerSteal(3, WOOD))))),
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 9)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 7)),
    Coproduct[PerfectInfoMoves](PerfectInfoRobberMoveResult(3, 9, Some(PlayerSteal(0, SHEEP)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult(PerfectInfoRobberMoveResult(0, 3, Some(PlayerSteal(3, BRICK))))),
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 5)),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 10)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 11)),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 10)),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(0, Vertex(17))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 11)),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(1, Vertex(50))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 5)),
    Coproduct[PerfectInfoMoves](PortTradeMove(2, ResourceSet(WHEAT, WHEAT, WHEAT, WHEAT), ResourceSet(WOOD))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(2, Vertex(24))),
    Coproduct[PerfectInfoMoves](PortTradeMove(2, ResourceSet(SHEEP, SHEEP), ResourceSet(WOOD))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(2, Edge(Vertex(29), Vertex(30)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 3)),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 10)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(0, Coproduct[DevelopmentCards](RoadBuilder))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult(PerfectInfoRobberMoveResult(1, 9, Some(PlayerSteal(2, WHEAT))))),
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 9)),
    Coproduct[PerfectInfoMoves](BuildRoadMove(1, Edge(Vertex(7), Vertex(8)))),
    Coproduct[PerfectInfoMoves](PortTradeMove(1, ResourceSet(br = 4), ResourceSet(wo = 1))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(1, Vertex(8))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 5)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 5)),
    Coproduct[PerfectInfoMoves](BuildCityMove(0, Vertex(41))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 5)),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 11)),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 9)),
    Coproduct[PerfectInfoMoves](BuildCityMove(0, Vertex(22))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 5)),
    Coproduct[PerfectInfoMoves](BuildCityMove(1, Vertex(34))),
    Coproduct[PerfectInfoMoves](PortTradeMove(1, ResourceSet(sh = 3), ResourceSet(wh = 1))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](Point))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 8)),
    Coproduct[PerfectInfoMoves](PortTradeMove(3, ResourceSet(sh = 3), ResourceSet(br = 1))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(3, Edge(Vertex(9), Vertex(10)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 7)),
    Coproduct[PerfectInfoMoves](DiscardMove(1, ResourceSet(or = 3, br = 1))),
    Coproduct[PerfectInfoMoves](PerfectInfoRobberMoveResult(0, 3, Some(PlayerSteal(1, ORE)))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(0, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 5)),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 8)),
    Coproduct[PerfectInfoMoves](PortTradeMove(2, ResourceSet(wh = 4), ResourceSet(wo = 1))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(2, Vertex(29))),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 5)),
    Coproduct[PerfectInfoMoves](PortTradeMove(3, ResourceSet(or = 3), ResourceSet(wh = 1))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(3, Coproduct[DevelopmentCards](Monopoly))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 10)),
    Coproduct[PerfectInfoMoves](PlayRoadBuilderMove(0, Edge(Vertex(20), Vertex(21)), Some(Edge(Vertex(39), Vertex(40))))),
    Coproduct[PerfectInfoMoves](PortTradeMove(0, ResourceSet(br = 2), ResourceSet(wo = 1))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(0, Vertex(20))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 5)),
    Coproduct[PerfectInfoMoves](PortTradeMove(1, ResourceSet(sh = 3), ResourceSet(wh = 1))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](YearOfPlenty))),
    Coproduct[PerfectInfoMoves](PortTradeMove(1, ResourceSet(br = 3), ResourceSet(wh = 1))),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](YearOfPlenty))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 8)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 6)),
    Coproduct[PerfectInfoMoves](PlayMonopolyMoveResult(3, WHEAT, Map(0 -> 6, 2 -> 3))),
    Coproduct[PerfectInfoMoves](PortTradeMove(3, ResourceSet(wh = 3), ResourceSet(or = 1))),
    Coproduct[PerfectInfoMoves](PortTradeMove(3, ResourceSet(wh = 3), ResourceSet(or = 1))),
    Coproduct[PerfectInfoMoves](PortTradeMove(3, ResourceSet(wh = 3), ResourceSet(or = 1))),
    Coproduct[PerfectInfoMoves](BuildCityMove(3, Vertex(31))),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 3)),
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult(PerfectInfoRobberMoveResult(0, 0, Some(PlayerSteal(3, WOOD))))),
    Coproduct[PerfectInfoMoves](PortTradeMove(0, ResourceSet(wo = 2), ResourceSet(wh = 1))),
    Coproduct[PerfectInfoMoves](BuildSettlementMove(0, Vertex(39))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(1, 10)),
    Coproduct[PerfectInfoMoves](PlayYearOfPlentyMove(1, ORE, WHEAT)),
    Coproduct[PerfectInfoMoves](PerfectInfoBuyDevelopmentCardMoveResult(1, Coproduct[DevelopmentCards](Knight))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(1, Edge(Vertex(35), Vertex(49)))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(1, Edge(Vertex(34), Vertex(35)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(1)),
    // 2
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(2, 11)),
    Coproduct[PerfectInfoMoves](EndTurnMove(2)),
    // 3
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(3, 6)),
    Coproduct[PerfectInfoMoves](EndTurnMove(3)),
    // 0
    Coproduct[PerfectInfoMoves](RollDiceMoveResult(0, 5)),
    Coproduct[PerfectInfoMoves](BuildRoadMove(0, Edge(Vertex(19), Vertex(20)))),
    Coproduct[PerfectInfoMoves](BuildRoadMove(0, Edge(Vertex(19), Vertex(42)))),
    Coproduct[PerfectInfoMoves](EndTurnMove(0)),
    // 1
    Coproduct[PerfectInfoMoves](PerfectInfoPlayKnightResult[Resource](PerfectInfoRobberMoveResult(1, 7, Some(PlayerSteal(0, WHEAT))))))

  val perfectResult = testMoveResults.foldLeft(initPerfectInfoState) { case (s, m) => BaseGame.perfectInfoBaseGame(m, s) }
  println(perfectResult.select[PlayerPoints])

  val transform = BaseGame.MoveTransformer()
  val transformedMoves = testMoveResults.map(transform.apply) ::: List(
    Coproduct[ImperfectInfoMoves](PlayPointMove(0)),
    Coproduct[ImperfectInfoMoves](PlayPointMove(1)),
    Coproduct[ImperfectInfoMoves](PlayPointMove(2)),
    Coproduct[ImperfectInfoMoves](PlayPointMove(3)))


  val publicResult = transformedMoves.foldLeft(initPublicInfoState) { case (s, m) => BaseGame.publicInfoBase(m, s)}
  println(publicResult.select[PlayerPoints])




  //    RevealPoint,
  //    EndTurnMove,
  //    //2
  //    RevealPoint,
  //    EndTurnMove,
  //    //3
  //    RevealPoint,
  //    EndTurnMove,
  //    //0
  //    RevealPoint,
  //    EndTurnMove,
}

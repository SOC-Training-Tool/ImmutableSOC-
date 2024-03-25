package soc.core

import game.{GameMove, GameMoveResult, InventorySet, PerfectInformationGameMove}

case class EndTurnMove(player: Int) extends PerfectInformationGameMove[EndTurnMove]

case class DiscardMove[II](player: Int, set: InventorySet[II, Int]) extends PerfectInformationGameMove[DiscardMove[II]]

case class TradeMove[II](player: Int, partner: Int, give: InventorySet[II, Int], get: InventorySet[II, Int]) extends PerfectInformationGameMove[TradeMove[II]]

case class RollDiceMove(player: Int) extends GameMove

case class RollDiceMoveResult(player: Int, result: Int) extends GameMoveResult {
  override type A = RollDiceMove

  override def move: A = RollDiceMove(player)
}

case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationGameMove[BuildCityMove]

case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationGameMove[BuildRoadMove]

case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationGameMove[BuildSettlementMove]

case class InitialPlacementMove(vertex: Vertex, edge: Edge, first: Boolean, player: Int) extends PerfectInformationGameMove[InitialPlacementMove]

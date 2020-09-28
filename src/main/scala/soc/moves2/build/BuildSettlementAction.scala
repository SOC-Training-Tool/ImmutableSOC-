package soc.moves2.build

import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory.{CatanSet, Inventory, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource, Settlement}
import soc.moves2.{GameAction, MoveResultProvider, PerfectInformationMoveGameAction, PerfectInformationSOCMove, SOCPlayerPointsMap, SOCState}
import util.MapWrapper

case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildSettlementMove]
case class BuildSettlementAction[BOARD <: BoardConfiguration, II <: Resource, STATE[P] <: SettlementSOCState[BOARD, P, II, STATE[P]]](limit: Int, cost: CatanSet[II, Int]) extends PerfectInformationMoveGameAction[BOARD, II, STATE, BuildSettlementMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    state.settlementsForPlayer(position) < limit && inv.itemSet.contains(cost)
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[BuildSettlementMove] = {
    state.board.vertices.map(BuildSettlementMove(position, _)).filter(state.canBuildSettlement)
  }
}

case class SOCSettlementMap(m: Map[Vertex, Settlement]) extends MapWrapper[Vertex, Settlement]
trait SettlementSOCState[BOARD <: BoardConfiguration, I <: InventoryItem, PERSPECTIVE <: InventoryHelper[I, PERSPECTIVE], STATE <: SettlementSOCState[BOARD, I, PERSPECTIVE, STATE]] extends SOCState[BOARD, I, PERSPECTIVE, STATE]{
  this: CitySOCState[BOARD, I, PERSPECTIVE, STATE] with RoadSOCState[BOARD, I, PERSPECTIVE, STATE] =>
  def self = this

  def settlements: SOCSettlementMap

  def updateSettlements(settlements: SOCSettlementMap): STATE

  def settlementsForPlayer(player: Int): Int = settlements.values.count(_.playerId == player)

  protected def canPlaceFreeSettlement(loc: Vertex): Boolean = {
    board.vertices.contains(loc) &&
      !(settlements.contains(loc) || cities.contains(loc))  &&
      board.neighboringVertices(loc).forall { v => !(settlements.contains(v) || cities.contains(v)) }
  }

  def canBuildSettlement(buildSettlementMove: BuildSettlementMove): Boolean = {
    canPlaceFreeSettlement(buildSettlementMove.vertex) && {
      board.edgesFromVertex(buildSettlementMove.vertex).exists { edge =>
        roads.get(edge).fold(false)(_.playerId == buildSettlementMove.player)
      }
    }
  }

  def buildSettlement(buildSettlementMove: BuildSettlementMove, buy: Option[CatanSet[I, Int]]): STATE = {
    val pointsForPlayer = playerPoints(buildSettlementMove.player)
    val us = updateSettlements(SOCSettlementMap(settlements + (buildSettlementMove.vertex -> Settlement(buildSettlementMove.player))))
    buy.fold(us)(cost => us.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildSettlementMove.player, cost))))
      .updatePoints(SOCPlayerPointsMap((playerPoints - buildSettlementMove.player) + (buildSettlementMove.player -> (pointsForPlayer + 1))))
  }
 }




package soc.moves2

import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.resources.ResourceSet.{ResourceSet, Resources}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.moves2.build.{BuildRoadAction, BuildRoadMove, BuildSettlementAction, BuildSettlementMove, RoadSOCState, SettlementSOCState}

case class InitialPlacementMove(player: Int, vertex: Vertex, edge: Edge) extends PerfectInformationSOCMove[InitialPlacementMove]
case class InitialPlacementAction[BOARD <: BoardConfiguration, STATE[P] <: InitialPlacementSOCState[BOARD, P, STATE[P]]](settlementAction: BuildSettlementAction[BOARD, Resource, STATE], roadAction: BuildRoadAction[BOARD, Resource, STATE]) extends PerfectInformationMoveGameAction[BOARD, Resource, STATE, InitialPlacementMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    state.self.settlementsForPlayer(position) < settlementAction.limit && state.self.roadsForPlayer(position) < roadAction.limit
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[InitialPlacementMove] = {
    state.board.vertices.flatMap { v =>
      state.board.edgesFromVertex.getOrElse(v, Nil).map ( e => InitialPlacementMove(position, v, e) )
    }.filter(state.canPlaceInitialSettlement)
  }
}

trait InitialPlacementSOCState[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: InitialPlacementSOCState[BOARD, PERSPECTIVE, STATE]] extends SOCState[BOARD, Resource, PERSPECTIVE, STATE] {
  this: SettlementSOCState[BOARD, Resource, PERSPECTIVE, STATE] with RoadSOCState[BOARD, Resource, PERSPECTIVE, STATE] =>
  def self = this

  def canPlaceInitialSettlement(initialPlacementMove: InitialPlacementMove): Boolean = {
    val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
    val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
    canPlaceFreeSettlement(initialPlacementMove.vertex) && {
      buildSettlement(buildSettlementMove, None).self.canBuildRoad(buildRoadMove)
    }
  }

  def placeInitialPlacement(initialPlacementMove: InitialPlacementMove): STATE = {
    val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
    val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
    val first = settlements.count(_._2.playerId == initialPlacementMove.player) < 1
    val resourcesFromSettlement: Resources = if (!first) {
      val resList = board.adjacentHexes(initialPlacementMove.vertex).flatMap { node =>
        node.hex.getResourceAndNumber.map { case (resource, _) => resource }
      }
      CatanSet.fromList(resList)
    } else CatanSet.empty[Resource, Int]
      buildRoad(buildRoadMove, None).self
        .buildSettlement(buildSettlementMove, None).self
        .updateTransactions(List(Gain(initialPlacementMove.player, resourcesFromSettlement), Lose(SOCState.BANK_PLAYER_ID, resourcesFromSettlement)))
  }
}

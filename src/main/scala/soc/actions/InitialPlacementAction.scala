package soc.actions

import game.GameAction
import shapeless.{::, Coproduct, HList}
import soc.actions.build.BuildRoadState._
import soc.actions.build.BuildSettlementState._
import soc.actions.build.{BuildRoadMove, BuildSettlementMove}
import soc.board.{Edge, Vertex}
import soc.core.{CORE_STATE, EdgeBuildingState, Resource, VertexBuildingState}
import soc.inventory.ResourceInventories
import util.DependsOn
import util.opext.Embedder


case class InitialPlacementMove(vertex: Vertex, edge: Edge, player: Int) extends PerfectInformationSOCMove[InitialPlacementMove]

class InitialPlacementAction[INV[_], VB <: Coproduct, EB <: Coproduct](implicit inv: ResourceInventories[Resource, INV[Resource]], settle: Embedder[VB, CSettlement], road: Embedder[EB, CRoad]) extends GameAction[InitialPlacementMove, VertexBuildingState[VB] :: EdgeBuildingState[EB] :: CORE_STATE[Resource, INV]] {
  override def applyMove[GAME_STATE <: HList](move: InitialPlacementMove, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, VertexBuildingState[VB] :: EdgeBuildingState[EB] :: CORE_STATE[Resource, INV]]): GAME_STATE = {
    implicit val settleInnerDep = dep.innerDependency[VertexBuildingState[VB] :: CORE_STATE[Resource, INV]]
    implicit val roadInnerDep = dep.innerDependency[EdgeBuildingState[EB] :: CORE_STATE[Resource, INV]]
    state
      .buildSettlement(BuildSettlementMove(move.player, move.vertex), None)
      .buildRoad(BuildRoadMove(move.player, move.edge), None)
  }
}

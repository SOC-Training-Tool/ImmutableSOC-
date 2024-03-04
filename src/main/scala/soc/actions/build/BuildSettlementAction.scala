package soc.actions.build

import game.{GameAction, InventorySet}
import shapeless.{:+:, ::, CNil, Coproduct, HList}
import soc.actions.PerfectInformationSOCMove
import soc.actions.build.BuildSettlementState.CSettlement
import soc.board.Vertex
import soc.core.ResourceSet.Resources
import soc.core.{CORE_STATE, Lose, PlayerBuilding, Resource, Settlement, VertexBuildingState}
import soc.inventory.ResourceInventories
import util.DependsOn
import util.opext.Embedder

case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildSettlementMove]

class BuildSettlementAction[INV[_], VB <: Coproduct](cost: Resources)(implicit inv: ResourceInventories[Resource, INV[Resource]], basis: Embedder[VB, CSettlement]) extends GameAction[BuildSettlementMove, VertexBuildingState[VB] :: CORE_STATE[Resource, INV]] {

  import BuildSettlementState._

  override def applyMove[GAME_STATE <: HList](move: BuildSettlementMove, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, VertexBuildingState[VB] :: CORE_STATE[Resource, INV]]): GAME_STATE = {
    state.buildSettlement(move, Some(cost))
  }
}

object BuildSettlementState {

  type CSettlement = Settlement.type :+: CNil

  implicit class BuildSettlementStateOps[II <: Coproduct, VB <: Coproduct, INV[_], STATE <: HList]
    (state: STATE)
    (implicit dep: DependsOn[STATE, VertexBuildingState[VB] :: CORE_STATE[II, INV]], inv: ResourceInventories[II, INV[II]], basis: Embedder[VB, CSettlement]) {

    import soc.core.CoreOps
    implicit val inner = dep.innerDependency[CORE_STATE[II, INV]]

    val settlements: Map[Vertex, Int] = dep.get[VertexBuildingState[VB]](state).map { case (v, PlayerBuilding(vb, p)) =>
        basis.deembed(vb).flatMap(_.select[Settlement.type]).map(_ => (v, p))
    }.flatten.toMap

    def addSettlement(vertex: Vertex, player: Int): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map + (vertex -> PlayerBuilding(basis.embed(Coproduct[CSettlement](Settlement)), player)), state)
    }

    def subtractSettlement(vertex: Vertex): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map - vertex, state)
    }

    def settlementVerticesForPlayer(player: Int): List[Vertex] = settlements.filter(_._2 == player).keys.toList
    def numSettlementsForPlayer(player: Int): Int = settlementVerticesForPlayer(player).size

    def buildSettlement(move: BuildSettlementMove, cost: Option[InventorySet[II, Int]]): STATE = {
      val result = addSettlement(move.vertex, move.player).incrementPointForPlayer(move.player)
      cost.fold(result)(c => result.updateResourceInventories(Lose[II](move.player, c)).addToBank(c))
    }

    def removeSettlement(vertex: Vertex): STATE = {
      settlements.get(vertex).fold(state)(subtractSettlement(vertex).decrementPointForPlayer)
    }
  }
}

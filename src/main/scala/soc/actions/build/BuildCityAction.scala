package soc.actions.build

import game.{GameAction, InventorySet}
import shapeless.{:+:, ::, CNil, Coproduct, HList}
import soc.actions.PerfectInformationSOCMove
import soc.actions.build.BuildCityState.CCity
import soc.actions.build.BuildSettlementState.CSettlement
import soc.board.Vertex
import soc.core.ResourceSet.Resources
import soc.core.{CORE_STATE, Lose, PlayerBuilding, Resource, Settlement, VertexBuildingState}
import soc.inventory.{City, ResourceInventories}
import util.DependsOn
import util.opext.Embedder

case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildCityMove]

class BuildCityAction[INV[_], VB <: Coproduct](cost: Resources)(implicit inv: ResourceInventories[Resource, INV[Resource]], basis: Embedder[VB, CCity]) extends GameAction[BuildCityMove, VertexBuildingState[VB] :: CORE_STATE[Resource, INV]] {

  import BuildCityState._

  override def applyMove[GAME_STATE <: HList](move: BuildCityMove, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, VertexBuildingState[VB] :: CORE_STATE[Resource, INV]]): GAME_STATE = {
    state.buildCity(move, Some(cost))
  }
}

object BuildCityState {

  type CCity = City.type :+: Settlement.type :+: CNil

  implicit class BuildCityStateOps[II <: Coproduct, VB <: Coproduct, INV[_], STATE <: HList]
    (state: STATE)
    (implicit dep: DependsOn[STATE, VertexBuildingState[VB] :: CORE_STATE[II, INV]], inv: ResourceInventories[II, INV[II]], basis: Embedder[VB, CCity]) {

    import BuildSettlementState.BuildSettlementStateOps
    import soc.core.CoreOps
    implicit val innerDep = dep.innerDependency[CORE_STATE[II, INV]]
    implicit val innerEmbed = basis.innerEmbed[CSettlement]

    val cities: Map[Vertex, Int] = dep.get[VertexBuildingState[VB]](state).map { case (v, PlayerBuilding(vb, p)) =>
      basis.deembed(vb).flatMap(_.select[City.type]).map(_ => (v, p))
    }.flatten.toMap

    def addCity(vertex: Vertex, player: Int): STATE = {
      val map = dep.get[VertexBuildingState[VB]](state)
      dep.update(map + (vertex -> PlayerBuilding(basis.embed(Coproduct[CCity](City)), player)), state)
    }

    def cityVerticesForPlayer(player: Int): List[Vertex] = cities.filter(_._2 == player).keys.toList
    def numCitiesForPlayer(player: Int): Int = cityVerticesForPlayer(player).size

    def buildCity(move: BuildCityMove, cost: Option[InventorySet[II, Int]]): STATE = {
      val result = state.removeSettlement(move.vertex)
          .addCity(move.vertex, move.player)
          .incrementPointForPlayer(move.player)
          .incrementPointForPlayer(move.player)
      cost.fold(result)(c => result.updateResourceInventories(Lose[II](move.player, c)).addToBank(c))
    }
  }
}

package soc.actions.build

import game.{GameAction, InventorySet}
import shapeless.{:+:, ::, CNil, Coproduct, HList}
import soc.actions.PerfectInformationSOCMove
import soc.actions.build.BuildRoadState.CRoad
import soc.board.Edge
import soc.core.ResourceSet.Resources
import soc.core.{CORE_STATE, EdgeBuildingState, Lose, PlayerBuilding, Resource, Road}
import soc.inventory.ResourceInventories
import util.DependsOn
import util.opext.Embedder

case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationSOCMove[BuildRoadMove]


class BuildRoadAction[INV[_], EB <: Coproduct](cost: Resources)(implicit inv: ResourceInventories[Resource, INV[Resource]], basis: Embedder[EB, CRoad]) extends GameAction[BuildRoadMove,  EdgeBuildingState[CRoad] :: CORE_STATE[Resource, INV]] {

  import BuildRoadState.BuildRoadStateOps
  override def applyMove[GAME_STATE <: HList](move: BuildRoadMove, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, EdgeBuildingState[CRoad] :: CORE_STATE[Resource, INV]]): GAME_STATE =
    BuildRoadStateOps(state).buildRoad(move, Some(cost))
}

object BuildRoadState {

  type CRoad = Road.type :+: CNil

  implicit class BuildRoadStateOps[II <: Coproduct, EB <: Coproduct, INV[_], STATE <: HList]
  (state: STATE)(implicit dep: DependsOn[STATE, EdgeBuildingState[EB] :: CORE_STATE[II, INV]], inv: ResourceInventories[II, INV[II]], basis: Embedder[EB, CRoad]) {

    import soc.core.CoreOps
    implicit val innerDep = dep.innerDependency[CORE_STATE[II, INV]]

    val roads = dep.get[EdgeBuildingState[EB]](state).map { case (v, PlayerBuilding(vb, p)) =>
      basis.deembed(vb).flatMap(_.select[Road.type]).map(_ => (v, p))
    }.flatten.toMap

    def addRoad(edge: Edge, player: Int): STATE = {
      val map = dep.get[EdgeBuildingState[EB]](state)
      dep.update(map + (edge -> PlayerBuilding(basis.embed(Coproduct[CRoad](Road)), player)), state)
    }

    def roadEdgesForPlayer(player: Int): List[Edge] = roads.filter(_._2 == player).keys.toList
    def numRoadsForPlayer(player: Int): Int = roadEdgesForPlayer(player).size

    def buildRoad(move: BuildRoadMove, cost: Option[InventorySet[II, Int]]): STATE = {
      val result = addRoad(move.edge, move.player)
      cost.fold(result)(c => result.updateResourceInventories(Lose[II](move.player, c)).addToBank(c))
    }
  }
}
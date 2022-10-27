package soc.state

import shapeless.{::, HList}
import soc.board.BoardConfiguration
import soc.inventory.{CatanSet, InventoryHelper, Resource}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.InitialPlacementMove
import soc.moves2.build.{BuildRoadMove, BuildSettlementMove}
import soc.state.SOCState.SOCState
import soc.state.build.{RoadBoardOps, SOCRoadMap, SOCSettlementMap, SettlementBoardOps}
import util.DependsOn

object InitialPlacementSOCState {

  implicit class InitialPlacementSOCStateOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE]) {

    import soc.state.build.RoadSOCState._
    import soc.state.SOCState._
    import soc.state.build.SettlementSOCState._

    implicit val settlementDp = dep.innerDependency[SOCSettlementMap :: SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val roadDp = dep.innerDependency[SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val socStateDp = dep.innerDependency[SOCState[BOARD, Resource, PERSPECTIVE]]

    def canPlaceInitialSettlement(initialPlacementMove: InitialPlacementMove): Boolean = {
      val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
      val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
      state.canPlaceFreeSettlement(initialPlacementMove.vertex) &&
        state.buildSettlement(buildSettlementMove, None).canBuildRoad(buildRoadMove)
    }

    def placeInitialPlacement(initialPlacementMove: InitialPlacementMove): STATE = {
      val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
      val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
      // TODO find better way to ensure first placement
      val first = state.settlements.m.count(_._2.playerId == initialPlacementMove.player) < 1
      val resourcesFromSettlement: CatanSet[Resource, Int] = if (!first) {
        val resList = state.board.adjacentHexes(initialPlacementMove.vertex).flatMap { node =>
          node.hex.getResourceAndNumber.map { case (resource, _) => resource }
        }
        CatanSet.fromList(resList)
      } else CatanSet.empty[Resource, Int]
      state.buildRoad(buildRoadMove, None)
        .buildSettlement(buildSettlementMove, None)
        .updateTransactions(List(Gain(initialPlacementMove.player, resourcesFromSettlement), Lose(SOCState.BANK_PLAYER_ID, resourcesFromSettlement)))
    }
  }
}

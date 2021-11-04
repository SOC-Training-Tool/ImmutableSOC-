package soc.moves2

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory._
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.SOCState.SOCState
import soc.moves2.build._

case class InitialPlacementMove(player: Int, first: Boolean, vertex: Vertex, edge: Edge) extends PerfectInformationSOCMove

object InitialPlacementMove {

  implicit def moveGenerator[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE]): MoveGenerator[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
    (state: STATE, _: PerfectInfo, pos: Int) =>
      import InitialPlacementSOCState._
      import SOCState._

      implicit val stateDep = dep.innerDependency[SOCState[BOARD, Resource, PERSPECTIVE]]
      val first = settlementOps.vertexBuildingMap(state).filter { case (_, v) => v.playerId == pos }.isEmpty
      state.board.vertices.flatMap { v =>
        state.board.edgesFromVertex.getOrElse(v, Nil).map(e => InitialPlacementMove(pos, first, v, e)).filter(state.canPlaceInitialSettlement)
      }
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCState[BOARD, II, PERSPECTIVE]]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
    (state, _, _) =>
      import SOCState._
      state.turn < 0
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove]): CanDoMove[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
    (state, inv, move) =>
      import InitialPlacementSOCState._
      canDoAction(state, inv, move.player) && state.canPlaceInitialSettlement(move)
  }

  type INIT_PLACEMENT_NEXT_MOVES[W[_ <: SOCMoveResult]] = W[InitialPlacementMove] :: W[RollDiceResult] :: HNil

  implicit def baseNextMoves[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], W[_ <: SOCMoveResult], A <: HList, STATE <: HList](implicit ws: Selector[A, W[InitialPlacementMove]], sa: SelectAll[A, INIT_PLACEMENT_NEXT_MOVES[W]], dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]): NextMove[BOARD, II, PERSPECTIVE, W, A, STATE, InitialPlacementMove] = (a: A) => {
    import SOCState._
    import SettlementSOCState._

    implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val func = {
      (state: STATE, r: InitialPlacementMove) =>
        if (r.player == state.playerIds.head && state.settlementsForPlayer(r.player) > 1) {

        }




        Map(r.player -> sa.apply(a).toList)
        ws.apply(a) -> func
    }
  }

  implicit def applyRollDiceMoveResult[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](implicit boardOps: BoardOps[BOARD, Resource, PERSPECTIVE, STATE], dep: DependsOn[STATE, SOCCanRollDice :: SOCRobberLocation :: SOCState[BOARD, Resource, PERSPECTIVE]]) = new ApplyMoveResult[BOARD, Resource, PERSPECTIVE, RollDiceResult, STATE] {
    override def apply(state: STATE, moveResult: RollDiceResult): STATE = {
      import RollDiceSOCState._
      state.rollDice(moveResult)
    }
  }

}

object InitialPlacementSOCState {

  implicit class InitialPlacementSOCStateOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE]) {

    import RoadSOCState._
    import SOCState._
    import SettlementSOCState._

    implicit val settlementDp = dep.innerDependency[SOCSettlementMap :: SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val roadDp = dep.innerDependency[SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]]
    implicit val socStateDp = dep.innerDependency[SOCState[BOARD, Resource, PERSPECTIVE]]

    def canPlaceInitialSettlement(initialPlacementMove: InitialPlacementMove): Boolean = {
      val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
      val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
      state.canPlaceFreeSettlement(initialPlacementMove.vertex) && {
        state.buildSettlement(buildSettlementMove, None).canBuildRoad(buildRoadMove)
      }
    }

    def placeInitialPlacement(initialPlacementMove: InitialPlacementMove): STATE = {
      val buildSettlementMove = BuildSettlementMove(initialPlacementMove.player, initialPlacementMove.vertex)
      val buildRoadMove = BuildRoadMove(initialPlacementMove.player, initialPlacementMove.edge)
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

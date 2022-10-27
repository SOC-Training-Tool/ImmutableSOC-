package soc.moves2

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory._
import soc.inventory.resources.{Gain, Lose}
import soc.state.SOCState.SOCState
import soc.moves2.build._
import soc.state.build.BoardOps
import soc.state.{SOCState, UpdateState}
import util.DependsOn

case class InitialPlacementMove(player: Int, first: Boolean, vertex: Vertex, edge: Edge) extends PerfectInformationSOCMove[InitialPlacementMove]

//object InitialPlacementMove {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE]): MoveGenerator[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
//    (state: STATE, _: PerfectInfo, pos: Int) =>
//      import InitialPlacementSOCState._
//      import soc.state.SOCState._
//
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, Resource, PERSPECTIVE]]
//      val first = settlementOps.vertexBuildingMap(state).filter { case (_, v) => v.playerId == pos }.isEmpty
//      state.board.vertices.flatMap { v =>
//        state.board.edgesFromVertex.getOrElse(v, Nil).map(e => InitialPlacementMove(pos, first, v, e)).filter(state.canPlaceInitialSettlement)
//      }
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCState[BOARD, II, PERSPECTIVE]]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
//    (state, _, _) =>
//      import soc.state.SOCState._
//      state.turn < 0
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove]): CanDoMove[BOARD, Resource, PERSPECTIVE, STATE, PerfectInfo, InitialPlacementMove] = {
//    (state, inv, move) =>
//      import InitialPlacementSOCState._
//      canDoAction(state, inv, move.player) && state.canPlaceInitialSettlement(move)
//  }
//
//  implicit def updateState[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCRoadMap :: SOCState[BOARD, Resource, PERSPECTIVE]], settlementOps: SettlementBoardOps[BOARD, Resource, PERSPECTIVE, STATE], roadOps: RoadBoardOps[BOARD, Resource, PERSPECTIVE, STATE]) = new UpdateState[BOARD, Resource, PERSPECTIVE, InitialPlacementMove, STATE] {
//    override def apply(t: STATE, u: InitialPlacementMove): STATE = {
//      import InitialPlacementSOCState._
//      t.placeInitialPlacement(u)
//    }
//  }
//}

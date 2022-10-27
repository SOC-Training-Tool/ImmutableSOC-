package soc.moves2.build

import shapeless.{HList, :: => :::}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory._
import soc.state.SOCState.{SOCState, SOCStateOps}
import soc.moves2._
import soc.state.build.BoardOps
import soc.state.{SOCPlayerPointsMap, SOCState, UpdateState}
import util.{DependsOn, MapWrapper}

case class BuildRoadMove(player: Int, edge: Edge) extends PerfectInformationSOCMove[BuildRoadMove]

//object RoadSOCState {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
//    (state: STATE, _: PerfectInfo, pos: Int) =>
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      state.board.edges.map(BuildRoadMove(pos, _)).filter(state.canBuildRoad)
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCCanRollDice ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildRoadMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
//    (state, inv, player) =>
//      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val cityDep = dep.innerDependency[SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val rollDep = dep.innerDependency[SOCCanRollDice ::: SOCState[BOARD, II, PERSPECTIVE]]
//      state.rolledDice && state.currentPlayer == player && state.roadsForPlayer(player) < 15 && inv.canSpend(cost.getCost)
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildRoadMove] = {
//    (state, inv, move) =>
//      canDoAction(state, inv, move.player) && state.canBuildRoad(move)
//  }
//
//  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildRoadMove]) = new UpdateState[BOARD, II, PERSPECTIVE, BuildRoadMove, STATE] {
//    override def apply(t: STATE, u: BuildRoadMove): STATE =  t.buildRoad(u, Some(cost.getCost))
//  }
//}

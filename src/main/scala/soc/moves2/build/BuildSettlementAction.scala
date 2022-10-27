package soc.moves2.build

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory._
import soc.state.SOCState._
import soc.moves2._
import soc.state.build.BoardOps
import soc.state.{SOCPlayerPointsMap, SOCState, UpdateState}
import util.{DependsOn, MapWrapper}
import util.hlist.WrappedSelectAll

case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildSettlementMove]

//object BuildSettlementAction {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
//    (state: STATE, _: PerfectInfo, pos: Int) =>
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      state.board.vertices.map(BuildSettlementMove(pos, _)).filter(state.canBuildSettlement)
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildSettlementMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
//    (state, inv, player) =>
//      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val cityDep = dep.innerDependency[SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]
//      state.rolledDice && state.currentPlayer == player && state.settlementsForPlayer(player) < 5 && inv.canSpend(cost.getCost)
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
//    (state, inv, move) =>
//      canDoAction(state, inv, move.player) && state.canBuildSettlement(move)
//  }
//
//  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildSettlementMove]): UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] = new UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] {
//    override def apply(t: STATE, u: BuildSettlementMove): STATE = t.buildSettlement(u, Some(cost.getCost))
//  }
//}

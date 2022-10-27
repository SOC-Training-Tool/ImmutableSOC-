package soc.moves2.build

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory._
import soc.moves2._
import soc.state.SOCState._
import soc.state.build.SOCCityMap
import soc.state.{UpdateState, SOCState => _}
import util.DependsOn

case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildCityMove]

//object BuildCityMove {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
//    (state: STATE, _: PerfectInfo, pos: Int) =>
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      state.board.vertices.map(BuildCityMove(pos, _)).filter(state.canBuildCity)
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildCityMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
//    (state, inv, player) =>
//
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val cityDep = dep.innerDependency[SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]]
//      implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]
//      state.rolledDice && state.currentPlayer == player && state.citiesForPlayer(player) < 4 && inv.canSpend(cost.getCost)
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
//    (state, inv, move) =>
//      canDoAction(state, inv, move.player) && state.canBuildCity(move)
//  }
//
//  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildCityMove]) = new UpdateState[BOARD, II, PERSPECTIVE, BuildCityMove, STATE] {
//    override def apply(t: STATE, u: BuildCityMove): STATE = t.buildCity(u, cost.getCost)
//  }
//}

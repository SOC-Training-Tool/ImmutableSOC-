package soc.moves2

import shapeless.{::, HList}
import soc.board.BoardConfiguration
import soc.inventory.{InventoryHelper, InventoryItem, PerfectInfoInventory}
import soc.state.SOCState._
import soc.state.UpdateState
import util.DependsOn

case class EndTurnMove(player: Int) extends PerfectInformationSOCMove[EndTurnMove]

//object EndTurnMove {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList]: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, EndTurnMove] = {
//    (_: STATE, _: PerfectInfo, pos: Int) => Seq(EndTurnMove(pos))
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, EndTurnMove] = {
//    (state, _, player) =>
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      state.rolledDice && state.currentPlayer == player
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, EndTurnMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, EndTurnMove] = {
//    (state, inv, move) => canDoAction(state, inv, move.player)
//  }
//}

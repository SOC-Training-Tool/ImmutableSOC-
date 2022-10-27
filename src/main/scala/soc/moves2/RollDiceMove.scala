package soc.moves2

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, BoardHex}
import soc.core.Roll
import soc.inventory._
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.state.SOCState.SOCState
import soc.state.SOCState
import soc.state.build.BoardOps
import util.DependsOn

import scala.util.Random

case class RollDiceMove(player: Int) extends SOCMove {
  type R = RollDiceResult
}

case class RollDiceResult(player: Int, roll: Roll) extends SOCMoveResult {
  override type A = RollDiceMove

  override def move: RollDiceMove = RollDiceMove(player)

  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, RollDiceResult] = playerIds.map(id => id -> this).toMap
}

//object RollDiceMove {
//
//  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList]: MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
//    (_: STATE, _: PerfectInfo, pos: Int) => Seq(RollDiceMove(pos))
//  }
//
//  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
//    (state, _, player) =>
//      import RollDiceSOCState._
//      import soc.state.SOCState._
//
//      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//      !state.rolledDice && state.currentPlayer == player
//  }
//
//  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, RollDiceMove] = {
//    (state, inv, move) => canDoAction(state, inv, move.player)
//  }
//
//  implicit def resultProvider[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE],STATE <: HList] = new MoveResultProvider[BOARD, II, PERSPECTIVE, STATE, RollDiceMove] {
//    override def getMoveResult(move: RollDiceMove, state: STATE): RollDiceResult = RollDiceResult(move.player, Roll(Random.between(1, 7) + Random.between(1, 7)))
//  }
//
//
//}

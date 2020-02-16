package soc.moves2

import soc.board.BoardConfiguration
import soc.inventory.Inventory
import soc.inventory.Inventory.PerfectInfo
import soc.state.{GamePhase, GameState}

trait SOCMove[A <: SOCMove[A]] {
  def applyToSTate[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PERSPECTIVE, BOARD]): GameState[PERSPECTIVE, BOARD]
}

abstract class GameAction[A <: SOCMove[A]] {
  def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): Seq[SOCMove[A]]
}

object GameAction {

  implicit def onPhase(phase: GamePhase): OnPhase = OnPhase(phase)

}

case class OnPhase(phases: GamePhase*) {
  def ||(phase: GamePhase) = OnPhase((phase :: phases.toList):_*)
  def eval(phase: GamePhase): Boolean = phases.exists(_ == phase)
}




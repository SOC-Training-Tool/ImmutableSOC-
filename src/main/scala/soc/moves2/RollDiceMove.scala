package soc.moves2

import soc.board.BoardConfiguration
import soc.core.Roll
import soc.dice.Dice
import soc.inventory.Inventory
import soc.inventory.Inventory.PerfectInfo
import soc.state.{GamePhase, GameState}

case class RollDiceMove() extends SOCMove[RollDiceMove]
case class RollDiceAction(dice: Dice) extends GameAction[RollDiceMove] {

  override def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): List[SOCMove[RollDiceMove]] = {
    if (state.phase == GamePhase.Roll) List(RollDiceMove())
    else Nil
  }

}
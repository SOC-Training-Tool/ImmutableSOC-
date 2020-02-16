package soc.moves2

import soc.board.BoardConfiguration
import soc.inventory.Inventory
import soc.inventory.Inventory.PerfectInfo
import soc.state.{GamePhase, GameState}

case class EndTurnMove() extends SOCMove[EndTurnMove]
case object EndTurnAction extends GameAction[EndTurnMove] {
  override def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): Seq[SOCMove[EndTurnMove]] = {
    if (state.phase == GamePhase.BuyTradeOrEnd) {
      List(EndTurnMove())
    } else Nil
  }
}

package soc.core.actions

import game.GameAction
import shapeless.{::, HNil}
import soc.core.EndTurnMove
import soc.core.state.Turn
import soc.core.state.ops.TurnOps

object EndTurnAction {

  def apply(): GameAction[EndTurnMove, Turn :: HNil] = {
    GameAction[EndTurnMove, Turn :: HNil] { case (_, state) =>
      state.incrementTurn
    }
  }
}
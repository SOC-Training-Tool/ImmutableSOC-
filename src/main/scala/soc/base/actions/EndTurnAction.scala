package soc.base.actions

import game.GameAction
import shapeless.{::, HNil}
import soc.base.EndTurnMove
import soc.base.state.Turn
import soc.base.state.ops.TurnOps

object EndTurnAction {

  def apply(): GameAction[EndTurnMove, Turn :: HNil] = {
    GameAction[EndTurnMove, Turn :: HNil] { case (_, state) =>
      state.incrementTurn
    }
  }
}
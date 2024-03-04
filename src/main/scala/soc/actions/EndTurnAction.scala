package soc.actions

import game.GameAction
import shapeless.HList
import soc.core.{CORE_STATE, Resource}
import soc.inventory.ResourceInventories
import util.DependsOn

case class EndTurnMove(player: Int) extends PerfectInformationSOCMove[EndTurnMove]

class EndTurnAction[INV[_]](implicit inv: ResourceInventories[Resource, INV[Resource]]) extends GameAction[EndTurnMove, CORE_STATE[Resource, INV]]{

  override def applyMove[GAME_STATE <: HList](move: EndTurnMove, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, CORE_STATE[Resource, INV]]): GAME_STATE = {
    import soc.core.CoreOps
    state.incrementTurn
  }
}

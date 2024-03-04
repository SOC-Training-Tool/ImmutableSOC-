package soc.actions

import game.{GameAction, InventorySet}
import shapeless.{Coproduct, HList}
import soc.core.{CORE_STATE, CoreOps, Gain, Lose}
import soc.inventory.ResourceInventories
import util.DependsOn

case class PortTradeMove[II <: Coproduct](player: Int, give: InventorySet[II, Int], get: InventorySet[II, Int]) extends PerfectInformationSOCMove[PortTradeMove[II]]

class PortTradeAction[II <: Coproduct, INV[_]](implicit inv: ResourceInventories[II, INV[II]]) extends GameAction[PortTradeMove[II], CORE_STATE[II, INV]] {
  override def applyMove[GAME_STATE <: HList](move: PortTradeMove[II], state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, CORE_STATE[II, INV]]): GAME_STATE = {
    state.updateResourceInventories(Lose[II](move.player, move.give), Gain[II](move.player, move.get))
      .addToBank(move.give).subtractFromBank(move.get)
  }
}

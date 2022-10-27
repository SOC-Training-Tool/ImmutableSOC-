package soc.state

import shapeless.{::, HList}
import soc.board.BoardConfiguration
import soc.inventory.{InventoryHelper, InventoryItem}
import soc.state.SOCState.SOCState
import util.DependsOn

case class SOCPlayersToDiscard(players: List[Int])

object DiscardSOCState {

  implicit class SOCDiscardStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCPlayersToDiscard :: SOCState[BOARD, II, PERSPECTIVE]]) {
    val playersToDiscard: SOCPlayersToDiscard = dep.get(state)

    def updatePlayersToDiscard(playersToDiscard: SOCPlayersToDiscard): STATE = dep.update(playersToDiscard, state)
  }
}

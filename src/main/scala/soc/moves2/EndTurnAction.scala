package soc.moves2

import soc.board.BoardConfiguration
import soc.inventory.{CatanSet, InventoryHelper, PerfectInfoInventory, Resource}

case class EndTurnMove(player: Int) extends PerfectInformationSOCMove[EndTurnMove]
case class EndTurnAction[BOARD <: BoardConfiguration, II <: Resource, STATE[P] <: SOCState[BOARD, P, II, STATE[P]]]() extends PerfectInformationMoveGameAction[BOARD, II, STATE, EndTurnMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = true
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[EndTurnMove] = List(EndTurnMove(position))
}


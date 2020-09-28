package soc.moves2

import soc.board.BoardConfiguration
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}

case class DiscardMove[II <: InventoryItem](player: Int, cards: CatanSet[II, Int]) extends PerfectInformationSOCMove[DiscardMove[II]]
case class DiscardAction[BOARD <: BoardConfiguration, II <: Resource, STATE[P] <: SOCState[BOARD, P, II, STATE[P]]](discardLimit: Int) extends PerfectInformationMoveGameAction[BOARD, II, STATE, DiscardMove[II]] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = inv.numCards > discardLimit
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[DiscardMove[II]] = {
    CatanSet.toList(inv.itemSet).combinations(inv.numCards / 2).map { resList =>
      DiscardMove(position, CatanSet.fromList(resList.toList))
    }
  }.distinct.toSeq
}

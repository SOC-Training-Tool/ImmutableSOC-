package soc.moves2

import shapeless.{HList, :: => :::}
import soc.board.BoardConfiguration
import soc.inventory.InventoryHelper.{PerfectInfoInv, PerfectInvHelper}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.moves2.SOCState._
import soc.moves2.build.{BuildSettlementMove, SettlementSOCState}
import soc.moves2.build.SettlementSOCState.SettlementSOCStateImplicits

case class DiscardMove[II <: InventoryItem](player: Int, cards: CatanSet[II, Int]) extends PerfectInformationSOCMove[DiscardMove[II]]

object DiscardMove {

  implicit def generator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: PerfectInvHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: SOCBaseOps0[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, DiscardMove[II]] =
    (_, perfectInfo, pos) => {
      val inv = perfectInfo
      CatanSet.toList(inv.itemSet).combinations(inv.numCards / 2).map { resList =>
        DiscardMove(pos, CatanSet.fromList(resList.toList))
      }.distinct.toSeq
    }

  implicit def canDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: PerfectInvHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit cardLimit: DiscardCardLimit[STATE[BOARD, II, PERSPECTIVE]]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, DiscardMove[II]] =
    (_, perfectInfo, _) => perfectInfo.numCards > cardLimit.cardLimit
}

trait DiscardCardLimit[S] {
  def cardLimit: Int
}



case class DiscardAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[B, I, P] <: HList](discardLimit: Int) extends PerfectInformationMoveGameAction[BOARD, II, STATE, DiscardAction[BOARD, II, STATE]] {
  override type A = DiscardMove[II]
  override type OpsImplicits[P] = SOCBaseOps1[BOARD, II, P, STATE, SOCPlayersToDiscard]

  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[BOARD, II, PERSPECTIVE], inv: PerfectInfo, position: Int)(implicit ops: OpsImplicits[PERSPECTIVE]): Boolean = true

  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[BOARD, II, PERSPECTIVE], inv: PerfectInfo, position: Int)(implicit ops: OpsImplicits[PERSPECTIVE]): Seq[DiscardMove[II]] = {
    CatanSet.toList(inv.itemSet).combinations(inv.numCards / 2).map { resList =>
      DiscardMove(position, CatanSet.fromList(resList.toList))
    }.distinct.toSeq
  }
}

case class SOCPlayersToDiscard(players: List[Int])

object SOCDiscardState {

  implicit class SOCDiscardStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE[B, I, P] <: HList](state: STATE[BOARD, II, PERSPECTIVE])(implicit dep: DependsOn[STATE[BOARD, II, PERSPECTIVE], SOCPlayersToDiscard ::: SOCState[BOARD, II, PERSPECTIVE]]) {

    val playersToDiscard: SOCPlayersToDiscard = dep.get(state)
    def updatePlayersToDiscard(playersToDiscard: SOCPlayersToDiscard): STATE[BOARD, II, PERSPECTIVE] = dep.update(playersToDiscard, state)
  }

}
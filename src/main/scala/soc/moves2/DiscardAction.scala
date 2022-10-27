package soc.moves2

import shapeless.{HList, :: => :::}
import soc.board.BoardConfiguration
import soc.inventory.InventoryHelper.{PerfectInfoInv, PerfectInvHelper}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.state.SOCState._
import soc.moves2.build.{BuildSettlementMove}
import soc.state.build.SettlementSOCState
import util.DependsOn

case class DiscardMove[II <: InventoryItem](player: Int, cards: CatanSet[II, Int]) extends PerfectInformationSOCMove[DiscardMove[II]]

//object DiscardMove {
//
//  implicit def generator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: PerfectInvHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: SOCBaseOps0[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, DiscardMove[II]] =
//    (_, perfectInfo, pos) => {
//      val inv = perfectInfo
//      CatanSet.toList(inv.itemSet).combinations(inv.numCards / 2).map { resList =>
//        DiscardMove(pos, CatanSet.fromList(resList.toList))
//      }.distinct.toSeq
//    }
//
//  implicit def canDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: PerfectInvHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit cardLimit: DiscardCardLimit[STATE[BOARD, II, PERSPECTIVE]]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, DiscardMove[II]] =
//    (_, perfectInfo, _) => perfectInfo.numCards > cardLimit.cardLimit
//}
//
//trait DiscardCardLimit[S] {
//  def cardLimit: Int
//}

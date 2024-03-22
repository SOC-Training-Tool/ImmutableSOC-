package soc.base.actions

import game.GameAction
import shapeless.{::, HNil}
import soc.base.DiscardMove
import soc.base.state.Bank
import soc.base.state.ops._
import soc.inventory.ResourceInventories
import soc.inventory.Transactions.PerfectInfo
import util.DependsOn


object DiscardAction {

  def apply[II, INV[_]](implicit inv: ResourceInventories[II, PerfectInfo[II], INV]) = {
    GameAction[DiscardMove[II], Bank[II] :: INV[II] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[Bank[II] :: INV[II] :: HNil]
      state.payToBank(move.player, move.set)
    }
  }
}

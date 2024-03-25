package soc.core.actions

import game.GameAction
import shapeless.{::, HNil}
import soc.core.Transactions.PerfectInfo
import soc.core.state.Bank
import soc.core.state.ops._
import soc.core.{DiscardMove, ResourceInventories}
import util.DependsOn

object DiscardAction {

  def apply[II, INV[_]](implicit inv: ResourceInventories[II, PerfectInfo[II], INV]) = {
    GameAction[DiscardMove[II], Bank[II] :: INV[II] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[Bank[II] :: INV[II] :: HNil]
      state.payToBank(move.player, move.set)
    }
  }
}

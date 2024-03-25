package soc.base.actions

import game.GameAction
import shapeless.{::, HNil}
import soc.base.PortTradeMove
import soc.core.ResourceInventories
import soc.core.Transactions.PerfectInfo
import soc.core.state.Bank
import soc.core.state.ops.BankInvOps
import util.DependsOn

object PortTradeAction {

  def apply[II, INV[_]](implicit inv: ResourceInventories[II, PerfectInfo[II], INV]): GameAction[PortTradeMove[II], Bank[II] :: INV[II] :: HNil] = {
    GameAction[PortTradeMove[II], Bank[II] :: INV[II] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[Bank[II] :: INV[II] :: HNil]
      state
        .payToBank(move.player, move.give)
        .getFromBank(move.player, move.get)
    }
  }
}

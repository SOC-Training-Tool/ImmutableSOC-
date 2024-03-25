package soc.core.actions

import game.GameAction
import shapeless.{::, Coproduct, HList, HNil}
import soc.core.ResourceInventories.ResourceInventoriesOp
import soc.core.{ResourceInventories, TradeMove}
import soc.core.Transactions.{Gain, Lose, PerfectInfo}
import util.DependsOn

object TradeAction {

  def apply[Res, Inv[_]](implicit inv: ResourceInventories[Res, PerfectInfo[Res], Inv]) = new GameAction[TradeMove[Res], Inv[Res] :: HNil] {
    override def applyMove[GAME_STATE <: HList](move: TradeMove[Res], state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, Inv[Res] :: HNil]): GAME_STATE = {
      dep.updateWith[Inv[Res]](state)(_.update(
        Coproduct[PerfectInfo[Res]](Lose(move.player, move.give)),
        Coproduct[PerfectInfo[Res]](Lose(move.partner, move.get)),
        Coproduct[PerfectInfo[Res]](Gain(move.player, move.get)),
        Coproduct[PerfectInfo[Res]](Gain(move.partner, move.give))
      ))
    }
  }

}

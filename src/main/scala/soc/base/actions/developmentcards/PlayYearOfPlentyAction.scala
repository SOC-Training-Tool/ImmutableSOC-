package soc.base.actions.developmentcards

import game.{GameAction, InventorySet}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.{PlayYearOfPlentyMove, YearOfPlenty}
import soc.core.state.ops.BankInvOps
import soc.core.state.{Bank, Turn}
import soc.core.{DevelopmentCardInventories, ResourceInventories, Transactions}
import util.DependsOn

object PlayYearOfPlentyAction {

  def apply[Res, ResInv[_], Dev <: Coproduct, DevInv[_]]
  (implicit res: ResourceInventories[Res, Transactions.PerfectInfo[Res], ResInv],
   dev: DevelopmentCardInventories[Dev, DevInv],
   inject: coproduct.Inject[Dev, YearOfPlenty.type]
  ): GameAction[PlayYearOfPlentyMove[Res], DevInv[Dev] :: Turn :: Bank[Res] :: ResInv[Res] :: HNil] = {
    GameAction[PlayYearOfPlentyMove[Res], Bank[Res] :: ResInv[Res] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[Bank[Res] :: ResInv[Res] :: HNil]
      val set = InventorySet.fromList(Seq(move.c1, move.c2))
      state.getFromBank(move.player, set)
    }.extend(PlayDevelopmentCardActionExtension[PlayYearOfPlentyMove[Res], Dev, YearOfPlenty.type, DevInv](YearOfPlenty))
  }
}

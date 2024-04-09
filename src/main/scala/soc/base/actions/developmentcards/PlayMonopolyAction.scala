package soc.base.actions.developmentcards

import game.{GameAction, InventorySet}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.{Monopoly, PlayMonopolyMoveResult}
import soc.core.ResourceInventories.ResourceInventoriesOp
import soc.core.Transactions.{Gain, Lose}
import soc.core.state.Turn
import soc.core.{DevelopmentCardInventories, ResourceInventories, Transactions}
import util.DependsOn

object PlayMonopolyAction {

  def apply[Res, ResInv[_], Dev <: Coproduct, DevInv[_]]
  (implicit res: ResourceInventories[Res, Transactions.PerfectInfo[Res], ResInv],
   dev: DevelopmentCardInventories[Dev, DevInv],
   inject: coproduct.Inject[Dev, Monopoly.type]
  ): GameAction[PlayMonopolyMoveResult[Res], DevInv[Dev] :: Turn :: ResInv[Res] :: HNil] = {
    GameAction[PlayMonopolyMoveResult[Res], ResInv[Res] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[ResInv[Res] :: HNil]
      val lose = move.cardsLost.map { case (p, cards) =>
        val set = InventorySet.fromMap(Map(move.res -> cards))
        Coproduct[Transactions.PerfectInfo[Res]](Lose(p, set))
      }.toList
      val totalLost = InventorySet.fromMap(Map(move.res -> move.cardsLost.values.sum))
      dep.updateWith[ResInv[Res]](state)(_.update(
        lose :+ Coproduct[Transactions.PerfectInfo[Res]](Gain(move.player, totalLost))
      ))
    }.extend(PlayDevelopmentCardActionExtension[PlayMonopolyMoveResult[Res], Dev, Monopoly.type, DevInv](Monopoly))
  }
}

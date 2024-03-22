package soc.base.actions.developmentcards

import game.{GameAction, InventorySet}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.PlayMonopolyMoveResult
import soc.base.state.{Bank, Turn}
import soc.inventory.ResourceInventories.ResourceInventoriesOp
import soc.inventory.Transactions.{Gain, Lose}
import soc.inventory.{DevelopmentCardInventories, ResourceInventories, Transactions}
import util.DependsOn

case object Monopoly

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
    }.extend(PlayDevelopmentCardActionExtension[PlayMonopolyMoveResult[Res], Dev, Monopoly.type, DevInv](_.player, Monopoly)).apply()
  }
}

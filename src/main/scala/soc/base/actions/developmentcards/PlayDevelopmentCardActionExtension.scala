package soc.base.actions.developmentcards

import game.ActionExtension
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.state.Turn
import soc.inventory.DevelopmentCardInventories
import soc.inventory.DevelopmentCardInventories.DevelopmentCardInventoriesOps

object PlayDevelopmentCardActionExtension {

  def apply[M, Dev <: Coproduct, Card, Inv[_]]
  (player: M => Int, card: Card)
  (implicit dev: DevelopmentCardInventories[Dev, Inv],
   inject: coproduct.Inject[Dev, Card]
  ): ActionExtension[M, Inv[Dev] :: Turn :: HNil] = new ActionExtension[M, Inv[Dev] :: Turn :: HNil] {
    override def apply(move: M, pre: STATE, post: STATE): STATE = {
      post.updateWith[Inv[Dev], Inv[Dev], STATE](_.playCard(player(move), Coproduct[Dev](card)))
    }
  }
}

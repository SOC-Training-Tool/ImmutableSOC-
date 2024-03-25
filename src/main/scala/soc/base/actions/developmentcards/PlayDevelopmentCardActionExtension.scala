package soc.base.actions.developmentcards

import game.{ActionExtension, GameMoveResult}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.core.DevelopmentCardInventories
import soc.core.DevelopmentCardInventories.DevelopmentCardInventoriesOps
import soc.core.state.Turn

object PlayDevelopmentCardActionExtension {

  def apply[M <: GameMoveResult, Dev <: Coproduct, Card, Inv[_]]
  (card: Card)
  (implicit dev: DevelopmentCardInventories[Dev, Inv],
   inject: coproduct.Inject[Dev, Card]
  ): ActionExtension[M, Inv[Dev] :: Turn :: HNil] = new ActionExtension[M, Inv[Dev] :: Turn :: HNil] {
    override def apply(move: M, pre: STATE, post: STATE): STATE = {
      post.updateWith[Inv[Dev], Inv[Dev], STATE](_.playCard(move.move.player, Coproduct[Dev](card)))
    }
  }
}

package soc.base.actions.developmentcards

import game.{ActionExtension, GameAction}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.{PerfectInfoBuyDevelopmentCardMoveResult, PlayPointMove, Point, state}
import soc.core.DevelopmentCardInventories
import soc.core.state.ops.PointsOps
import soc.core.state.{PlayerPoints, Turn}

object PlayPointAction {

  def apply[Dev <: Coproduct, DevInv[_]]
  (implicit dev: DevelopmentCardInventories[Dev, DevInv],
   inject: coproduct.Inject[Dev, Point.type],
  ): GameAction[PlayPointMove, DevInv[Dev] :: Turn :: PlayerPoints :: HNil] = {
    GameAction[PlayPointMove, PlayerPoints :: HNil] { case (move, state) =>
      state.incrementPointForPlayer(move.player)
    }.extend(PlayDevelopmentCardActionExtension[PlayPointMove, Dev, Point.type, DevInv](Point))
  }

  def extension[Dev <: Coproduct](implicit selector: coproduct.Selector[Dev, Point.type]) = {
    new ActionExtension[PerfectInfoBuyDevelopmentCardMoveResult[Dev], PlayerPoints :: HNil] {
      override def apply(move: PerfectInfoBuyDevelopmentCardMoveResult[Dev], pre: STATE, post: STATE): STATE = {
        move.card.select[Point.type].fold(post)(_ => post.incrementPointForPlayer(move.player))
      }
    }
  }
}

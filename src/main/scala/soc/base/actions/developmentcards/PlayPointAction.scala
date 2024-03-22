package soc.base.actions.developmentcards

import game.{ActionExtension, GameAction, PerfectInformationGameMove}
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.{PerfectInfoBuyDevelopmentCardMoveResult, PlayPointMove, state}
import soc.base.state.PlayerPoints
import soc.base.state.ops.PointsOps
import soc.core
import soc.inventory.{DevTransactions, DevelopmentCardInventories, ResourceInventories}
import util.DependsOn

case object Point

object PlayPointAction {

  def apply[Dev <: Coproduct, DevInv[_]]
  (implicit dev: DevelopmentCardInventories[Dev, DevInv],
   inject: coproduct.Inject[Dev, Point.type],
  ): GameAction[PlayPointMove, DevInv[Dev] :: state.Turn :: PlayerPoints :: HNil] = {
    GameAction[PlayPointMove, PlayerPoints :: HNil] { case (move, state) =>
      state.incrementPointForPlayer(move.player)
    }.extend(PlayDevelopmentCardActionExtension[PlayPointMove, Dev, Point.type, DevInv](_.player, Point)).apply()
  }

  def extension[Dev <: Coproduct](implicit selector: coproduct.Selector[Dev, Point.type]) = {
    new ActionExtension[PerfectInfoBuyDevelopmentCardMoveResult[Dev], PlayerPoints :: HNil] {
      override def apply(move: PerfectInfoBuyDevelopmentCardMoveResult[Dev], pre: STATE, post: STATE): STATE = {
        move.card.select[Point.type].fold(post)(_ => post.incrementPointForPlayer(move.player))
      }
    }
  }
}

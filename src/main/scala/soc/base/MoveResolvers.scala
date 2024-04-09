package soc.base

import shapeless.HList
import shapeless.ops.hlist
import shapeless.ops.hlist.Selector
import soc.MoveResolver
import soc.MoveResolver._
import soc.base.BaseGame.{BaseMovesDevelopmentCard, PerfectInfoMoves, PerfectInfoState, perfectInfoBaseGame}
import soc.base.state.DevelopmentCardDeck
import soc.core.ResourceInventories.PrivateInventories
import soc.core.{RollDiceMove, RollDiceMoveResult}

import scala.util.Random

object MoveResolvers {

  implicit def rollDice[S](implicit random: Random): Aux[RollDiceMove, S, RollDiceMoveResult] =
    new MoveResolver[RollDiceMove, S] {
      type Out = RollDiceMoveResult

      private def diceRoll = random.nextInt(6) + 1

      override def apply(t: RollDiceMove, u: S): RollDiceMoveResult = RollDiceMoveResult(t.player, diceRoll + diceRoll)
    }

  implicit def moveRobberAndSteal[Res, S <: HList]
  (implicit random: Random, selector: Selector[S, PrivateInventories[Res]]): Aux[RobberMove[Res], S, PerfectInfoRobberMoveResult[Res]] =
    new MoveResolver[RobberMove[Res], S] {
      type Out = PerfectInfoRobberMoveResult[Res]

      override def apply(move: RobberMove[Res], state: S): PerfectInfoRobberMoveResult[Res] = {
        val steal = for {
          victim <- move.victim
          inv <- state.select[PrivateInventories[Res]].get(victim)
          resource <- random.shuffle(inv.amountMap.flatMap { case (res, amt) =>
            (0 to amt).map(_ => res).toList
          }.toList).headOption
        } yield PlayerSteal(victim, resource)
        PerfectInfoRobberMoveResult[Res](move.player, move.robberHexId, steal)
      }
    }

  implicit def playKnight[Res, S <: HList](implicit moveRobber: Aux[RobberMove[Res], S, PerfectInfoRobberMoveResult[Res]]): Aux[PlayKnightMove[Res], S, PerfectInfoPlayKnightResult[Res]] = {
    new MoveResolver[PlayKnightMove[Res], S] {
      type Out = PerfectInfoPlayKnightResult[Res]

      override def apply(t: PlayKnightMove[Res], u: S): PerfectInfoPlayKnightResult[Res] =
        PerfectInfoPlayKnightResult(moveRobber.apply(t.inner, u))
    }
  }

  implicit def buyDev[Dev, S <: HList](implicit selector: Selector[S, DevelopmentCardDeck[Dev]]): Aux[BuyDevelopmentCardMove[Dev], S, PerfectInfoBuyDevelopmentCardMoveResult[Dev]] =
    new MoveResolver[BuyDevelopmentCardMove[Dev], S] {
      override type Out = PerfectInfoBuyDevelopmentCardMoveResult[Dev]

      override def apply(move: BuyDevelopmentCardMove[Dev], state: S): Out = {
        PerfectInfoBuyDevelopmentCardMoveResult(move.player, state.select[DevelopmentCardDeck[Dev]].cards.head)
      }
    }

  implicit def monopoly[Res, S <: HList](implicit selector: Selector[S, PrivateInventories[Res]]): Aux[PlayMonopolyMove[Res], S, PlayMonopolyMoveResult[Res]] =
    new MoveResolver[PlayMonopolyMove[Res], S] {
      type Out = PlayMonopolyMoveResult[Res]

      override def apply(move: PlayMonopolyMove[Res], state: S): PlayMonopolyMoveResult[Res] = {
        val cardsLost = state.select[PrivateInventories[Res]]
          .filterNot(_._1 == move.player)
          .map { case (p, pInv) => p -> pInv.getAmount(move.res) }
        PlayMonopolyMoveResult(move.player, move.res, cardsLost)
      }
    }

  implicit def baseGameMoveResolver[S <: HList](implicit random: Random, selectAll: hlist.SelectAll[S, PerfectInfoState]): Aux[BaseMovesDevelopmentCard, S, PerfectInfoMoves] =
    new MoveResolver[BaseMovesDevelopmentCard, S] {
      override type Out = PerfectInfoMoves

      private val perfectInfoStateMoveResolver: Aux[BaseMovesDevelopmentCard, PerfectInfoState, PerfectInfoMoves] =
        implicitly[Aux[BaseMovesDevelopmentCard, PerfectInfoState, PerfectInfoMoves]]

      override def apply(t: BaseMovesDevelopmentCard, u: S): Out =
        perfectInfoStateMoveResolver.apply(t, selectAll.apply(u))
    }
}

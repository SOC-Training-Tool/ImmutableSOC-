package soc.base.actions.special

import game.{ActionExtension, GameMoveResult}
import shapeless.{::, HNil}
import soc.base.PlayKnightMove
import soc.base.state.ops._
import soc.base.state.{LargestArmyPlayer, PlayerArmyCount, PlayerPoints}
import util.DependsOn

object LargestArmyExtension {

  def apply[M <: GameMoveResult.Aux[PlayKnightMove]] = {
    new ActionExtension[M, LargestArmyPlayer :: PlayerArmyCount :: PlayerPoints :: HNil] {

      override def apply(move: M, pre: STATE, post: STATE): STATE = {
        implicit val pointsDep = DependsOn[STATE, PlayerPoints :: HNil]
        implicit val armyDep = DependsOn[STATE, LargestArmyPlayer :: PlayerArmyCount :: HNil]
        val player = move.move.player
        val largestArmyPlayer = pre.largestArmyPlayer
        val updatedArmyCount = {
          val currArmySize = post.armyCount
          PlayerArmyCount(currArmySize + (player -> (currArmySize.getOrElse(player, 0) + 1)))
        }
        val updatedArmyPlayer = updatedSpecialPlayer(3, largestArmyPlayer, updatedArmyCount.m)
        val result = post.updateArmyCount(updatedArmyCount.m)
        updateState[STATE](largestArmyPlayer, updatedArmyPlayer, result)(
          _.incrementPointForPlayer(_),
          _.decrementPointForPlayer(_),
          _.updateLargestArmyPlayer(_))
      }
    }
  }
}

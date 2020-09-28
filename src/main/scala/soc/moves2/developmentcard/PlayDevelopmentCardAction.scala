package soc.moves2.developmentcard

import soc.board.BoardConfiguration
import soc.inventory.{DevelopmentCard, InventoryItem}
import soc.moves2.{GameAction, PerfectInformationSOCMove, SOCMove, SOCMoveResult}

trait PlayDevelopmentCardMove[A <: PlayDevelopmentCardMove[A]] extends SOCMove[A] {
  def card: DevelopmentCard
}
trait PlayDevelopmentCardMoveResult[A <: PlayDevelopmentCardMove[A]] extends SOCMoveResult[A]
trait PerfectPlayDevelopmentCardMove[A <: PlayDevelopmentCardMove[A]] extends PlayDevelopmentCardMove[A] with PlayDevelopmentCardMoveResult[A] with PerfectInformationSOCMove[A]

trait PlayDevelopmentCardAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: DevelopmentCardSOCState[BOARD, II, P, STATE[P]], A <: PlayDevelopmentCardMove[A]] extends GameAction[BOARD, II, STATE, A] {
  val cardsInDeck: Int
  val cardType: DevelopmentCard

  def canDoAction[PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PublicInfoDevelopmentCardInventory[II]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    inv.canPlayCard(cardType, state.turn.t)
  }
}

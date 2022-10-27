//package soc.moves2.developmentcard
//
//import soc.board.BoardConfiguration
//import soc.inventory.{DevelopmentCard, InventoryItem}
//import soc.moves2.{GameAction, MoveGenerator, PerfectInformationSOCMove, SOCMove, SOCMoveResult}
//
//trait PlayDevelopmentCardMove extends SOCMove {
//  def card: DevelopmentCard
//}
//trait PlayDevelopmentCardMoveResult[A <: PlayDevelopmentCardMove] extends SOCMoveResult[A]
//trait PerfectPlayDevelopmentCardMove[A <: PlayDevelopmentCardMove] extends PerfectInformationSOCMove[A]
//  with PlayDevelopmentCardMove with PlayDevelopmentCardMoveResult[A] { self: A => }
//
//trait PlayDevelopmentCardMoveGenerator[S, A <: PlayDevelopmentCardMove] extends MoveGenerator[S, A]
//
//
//trait PlayDevelopmentCardMoveResult[A <: PlayDevelopmentCardMove[A]] extends SOCMoveResult[A]
//trait PerfectPlayDevelopmentCardMove[A <: PlayDevelopmentCardMove[A]] extends PlayDevelopmentCardMove[A] with PlayDevelopmentCardMoveResult[A] with PerfectInformationSOCMove[A]
//
//trait PlayDevelopmentCardAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: DevelopmentCardSOCState[BOARD, II, P, STATE[P]], M <: PlayDevelopmentCardMove[M], GA <: PlayDevelopmentCardAction[BOARD, II, STATE, M, GA]] extends GameAction[BOARD, II, STATE, GA] { this: GA =>
//  val cardsInDeck: Int
//  val cardType: DevelopmentCard
//
//  override type A = M
//  def canDoAction[PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], PerfectInfo <: PublicInfoDevelopmentCardInventory[II]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
//    inv.canPlayCard(cardType, state.turn.t)
//  }
//}

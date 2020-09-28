package soc.moves2.developmentcard

import soc.board.BoardConfiguration
import soc.inventory.{CatanSet, DevelopmentCard, InventoryHelper, InventoryItem, PerfectInfoInventory}
import soc.moves2.{GameAction, MoveResultProvider, SOCMove, SOCMoveResult, SOCState}

case class BuyDevelopmentCardMove(player: Int) extends SOCMove[BuyDevelopmentCardMove]
case class BuyDevelopmentCardsMoveResult(player: Int, card: Option[DevelopmentCard]) extends SOCMoveResult[BuyDevelopmentCardMove] {
  override def move: BuyDevelopmentCardMove = BuyDevelopmentCardMove(player)
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[BuyDevelopmentCardMove]] = playerIds.map {
    case `player` => player -> this
    case p => p -> BuyDevelopmentCardsMoveResult(player, None)
  }.toMap
}
case class BuyDevelopmentCardAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: DevelopmentCardSOCState[BOARD, II, P, STATE[P]]](cost: CatanSet[II, Int], playDevelopmentCardActions: PlayDevelopmentCardAction[BOARD, II, STATE, _]*)(implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, BuyDevelopmentCardMove, BuyDevelopmentCardsMoveResult]) extends GameAction[BOARD, II, STATE, BuyDevelopmentCardMove] {
  val developmentCardDeck = playDevelopmentCardActions.map(_.cardsInDeck).sum
  override type R = BuyDevelopmentCardsMoveResult
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    val devCardsDeck = state.developmentCardsLeft
    inv.canSpend(cost) && devCardsDeck > 0
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[BuyDevelopmentCardMove] =  List(BuyDevelopmentCardMove(position))
}

case class SOCDevelopmentCardsInDeck(d: Int)
trait DevelopmentCardSOCState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: DevelopmentCardInventoryHelper[II, PERSPECTIVE], STATE <: DevelopmentCardSOCState[BOARD, II, PERSPECTIVE, STATE]] extends SOCState[BOARD, II, PERSPECTIVE, STATE] {
  def developmentCardsLeft: SOCDevelopmentCardsInDeck
  def decrementNumDevelopmentCards: STATE

  def buyDevelopmentCard(buy: BuyDevelopmentCardsMoveResult): STATE = updateInvHelper(inventoryHelper.buyDevelopmentCard(buy.player, turn.t, buy.card)).decrementNumDevelopmentCards
  def playDevelopmentCard[A <: PlayDevelopmentCardMove[A]](card: PlayDevelopmentCardMoveResult[A]): STATE = updateInvHelper(inventoryHelper.playDevelopmentCard(card.move.player, turn.t, card.move.card))
}







package soc.moves2

import soc.board.BoardConfiguration
import soc.inventory.{DevelopmentCard, Inventory}
import soc.inventory.Inventory.PerfectInfo
import soc.inventory.resources.ResourceSet.Resources
import soc.state.{GamePhase, GameState}

case class BuyDevelopmentCardMove() extends SOCMove[BuyDevelopmentCardMove]
case class BuyDevelopmentCardAction(cost: Resources, playDevelopmentCardActions: PlayDevelopmentCardAction[_]*) extends GameAction[BuyDevelopmentCardMove] {

  val developmentCardDeck = playDevelopmentCardActions.map(_.cardsInDeck).sum

  override def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): Seq[SOCMove[BuyDevelopmentCardMove]] = {
    val devCardsDeck = state.developmentCardsLeft
    if ((state.phase == GamePhase.Roll || state.phase == GamePhase.BuyTradeOrEnd) && inv.canSpend(cost) && devCardsDeck > 0)
      List(BuyDevelopmentCardMove())
    else Nil
  }
}






package soc.moves2.developmentcard

import soc.board.BoardConfiguration
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory.{DevelopmentCard, Inventory, InventoryHelper, Monopoly, PerfectInfoInventory, Resource}
import soc.moves2.{MoveResultProvider, SOCMove, SOCMoveResult}
import soc.state.{GameState, SOCState}

case class PlayMonopolyMove(player: Int, res: Resource) extends PlayDevelopmentCardMove[PlayMonopolyMove] {
  override def card: DevelopmentCard = Monopoly
}

case class PlayMonopolyMoveResult(move: PlayMonopolyMove, cardsLost: Map[Int, Resources]) extends SOCMoveResult[PlayMonopolyMove] {
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[PlayMonopolyMove]] = playerIds.map(_ -> this).toMap
}
case class PlayMonopolyAction[BOARD <: BoardConfiguration, STATE[P] <: SOCState[BOARD, Resource, P, STATE[P]]](cardsInDeck: Int, moveResultProvider: MoveResultProvider[BOARD, Resource, STATE, PlayMonopolyMove, PlayMonopolyMoveResult]) extends PlayDevelopmentCardAction[BOARD, Resource, STATE, PlayMonopolyMove, PlayMonopolyAction[BOARD, STATE]] {
  override val cardType: DevelopmentCard = Monopoly
  override type R = PlayMonopolyMoveResult

  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PlayMonopolyMove] = {
    Resource.list.map { PlayMonopolyMove(position, _) }
  }
}

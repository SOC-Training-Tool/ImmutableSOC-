package soc.moves2.developmentcard

import soc.board.BoardConfiguration
import soc.inventory.{DevelopmentCard, InventoryHelper, PerfectInfoInventory, Resource, YearOfPlenty}
import soc.moves2.{MoveResultProvider, PerfectInformationMoveGameAction, PerfectInformationSOCMove, SOCState}

case class PlayYearOfPlentyMove(player: Int, res1: Resource, res2: Resource) extends PerfectPlayDevelopmentCardMove[PlayYearOfPlentyMove] {
  override def card: DevelopmentCard = YearOfPlenty
}

case class PlayYearOfPlentyAction[BOARD <: BoardConfiguration, STATE[P] <: SOCState[BOARD, Resource, P, STATE[P]]](cardsInDeck: Int) extends PlayDevelopmentCardAction[BOARD, Resource, STATE, PlayYearOfPlentyMove] with PerfectInformationMoveGameAction[BOARD, Resource, STATE, PlayYearOfPlentyMove]{
  override val cardType: DevelopmentCard = YearOfPlenty
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PlayYearOfPlentyMove] = {
    Resource.list.flatMap { res1 =>
      Resource.list.map { res2 =>
        PlayYearOfPlentyMove(position, res1, res2)
      }
    }.foldLeft(List.empty[PlayYearOfPlentyMove]) {
      case (yopList, PlayYearOfPlentyMove(_, res1, res2)) if yopList.contains(PlayYearOfPlentyMove(position, res2, res1)) || yopList.contains(position, PlayYearOfPlentyMove(position, res1, res2)) => yopList
      case (yopList, move) => move :: yopList
    }
  }
}
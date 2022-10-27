package soc.moves2.developmentcard

import shapeless.HList
import soc.board.BoardConfiguration
import soc.inventory.{DevelopmentCard, InventoryHelper, PerfectInfoInventory, Resource, YearOfPlenty}
import soc.moves2.{EndTurnMove, MoveGenerator, MoveResultProvider, PerfectInformationMoveGameAction, PerfectInformationSOCMove}
import soc.state.SOCState

case class PlayYearOfPlentyMove(player: Int, res1: Resource, res2: Resource) extends PerfectPlayDevelopmentCardMove[PlayYearOfPlentyMove] {
  override def card: DevelopmentCard = YearOfPlenty
}

object PlayYearOfPlentyMove {

  implicit def generator[BOARD <: BoardConfiguration, PERSPECTIVE <: DevelopmentCardInventoryHelper[Resource, PERSPECTIVE], STATE[B, I, P] <: HList]: MoveGenerator[STATE[BOARD, Resource, PERSPECTIVE], PlayYearOfPlentyMove] =
    (pos: Int, _) => {
      Resource.list.flatMap { res1 =>
        Resource.list.map { res2 =>
          PlayYearOfPlentyMove(pos, res1, res2)
        }
      }.foldLeft(List.empty[PlayYearOfPlentyMove]) {
        case (yopList, PlayYearOfPlentyMove(_, res1, res2)) if yopList.contains(PlayYearOfPlentyMove(pos, res2, res1)) || yopList.contains(pos, PlayYearOfPlentyMove(pos, res1, res2)) => yopList
        case (yopList, move) => move :: yopList
      }
    }
}


case class PlayYearOfPlentyAction[BOARD <: BoardConfiguration, STATE[P] <: SOCState[BOARD, Resource, P, STATE[P]]](cardsInDeck: Int) extends PlayDevelopmentCardAction[BOARD, Resource, STATE, PlayYearOfPlentyMove, PlayYearOfPlentyAction[BOARD, STATE]] with PerfectInformationMoveGameAction[BOARD, Resource, STATE, PlayYearOfPlentyAction[BOARD, STATE]]{
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
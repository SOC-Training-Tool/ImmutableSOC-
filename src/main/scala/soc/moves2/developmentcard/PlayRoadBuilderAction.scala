package soc.moves2.developmentcard

import soc.board.{BoardConfiguration, CatanBoard, Edge}
import soc.inventory.{DevelopmentCard, Inventory, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource, RoadBuilder, YearOfPlenty}
import soc.moves.RoadBuilderMove
import soc.moves2.{MoveResultProvider, PerfectInformationMoveGameAction, PerfectInformationSOCMove, SOCMove, SOCMoveResult, SOCState}
import soc.moves2.build.{BuildRoadAction, BuildRoadMove, RoadSOCState}
import soc.state.GameState

case class PlayRoadBuilderMove(player: Int, road1: Edge, road2: Option[Edge]) extends PerfectPlayDevelopmentCardMove[PlayRoadBuilderMove] {
  override def card: DevelopmentCard = RoadBuilder
}
case class PlayRoadBuilderAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: RoadSOCState[BOARD, II, P, STATE[P]]](cardsInDeck: Int, buildRoadAction: BuildRoadAction[BOARD, II, STATE]) extends PlayDevelopmentCardAction[BOARD, II, STATE, PlayRoadBuilderMove, PlayRoadBuilderAction[BOARD, II, STATE]] with PerfectInformationMoveGameAction[BOARD, Resource, STATE, PlayRoadBuilderAction[BOARD, II, STATE]]{
  override val cardType: DevelopmentCard = RoadBuilder

  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PlayRoadBuilderMove] = {
    val firstRoadAndBoard: Seq[(Edge, STATE[PERSPECTIVE])] = buildRoadAction.getAllMoves(state, inv, position).map { road1 =>
      (road1.getMove.edge, state.buildRoad(road1, None))
    }
    if (state.board.getNumRoadsForPlayer(state.currentPlayer) < buildRoadAction.limit - 1) {
      firstRoadAndBoard.flatMap {case (road1, newBoard) =>
        buildRoadAction.getAllMoves(newBoard, inv, position).map { road2 =>
          PlayRoadBuilderMove(position, road1, Some(road2.edge))
        }
      }
    } else firstRoadAndBoard.map { case (edge, _) => PlayRoadBuilderMove(position, edge, None)}
  }
}


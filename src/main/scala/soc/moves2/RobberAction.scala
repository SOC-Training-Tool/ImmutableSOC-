package soc.moves2

import shapeless.{::, HList}
import shapeless.ops.hlist.SelectAll
import shapeless.ops.record.Updater
import soc.board.{BoardConfiguration, CatanBoard, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.inventory.resources.{ResourceSet, SOCTransactions, Steal}
import soc.state.SOCState._
import soc.moves2.build.{BuildSettlementMove}
import soc.moves2.developmentcard.{BuyDevelopmentCardMove, BuyDevelopmentCardsMoveResult, DevelopmentCardInventoryHelper, SOCDevelopmentCardsInDeck}
import soc.state.build.{BoardOps, CityBoardOps, SOCCityMap, SOCSettlementMap, SettlementSOCState}
import util.DependsOn

case class RobberMove[II <: InventoryItem](player: Int, robberLocation: Int, playerStole: Option[Int]) extends SOCMove {
  override type R = RobberMoveResult[II]
}

case class RobPlayer[II <: InventoryItem](player: Int, res: Option[II])
case class RobberMoveResult[II <: InventoryItem](player: Int, robberLocation: Int, cardStole: Option[RobPlayer[II]]) extends SOCMoveResult {
  override type A = RobberMove[II]
  override def move: RobberMove[II] = RobberMove(player, robberLocation, cardStole.map(_.player))
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult] = playerIds.map {
    case `player` => player -> this
    case p if cardStole.fold(false)(_.player == p) => p -> this
    case p => p -> RobberMoveResult[II](player, cardStole.map(_.copy(res = None)))
  }.toMap
}

//object RobberMove {
//
//  implicit def generator[BOARD <: BoardConfiguration, II <: Resource, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: RobberSOCStateImplicits[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, RobberMove] =
//    (state, _, pos) => {
//      import soc.state.SOCState._
//      import RobberSOCState._
//      implicit val (op1, boardOps) = ops
//      import op1._
//
//      val board = state.board
//      board.hexesWithNodes.filterNot(_.node == state.robberLocation).flatMap { hex =>
//        hex.vertices.flatMap(boardOps.vertexBuildingMap(state).get).map(_.playerId).distinct
//          .filterNot(p => p == pos || state.playerInventories.get(p).fold(false)(_.numCards <= 0)) match {
//            case Nil => List(RobberMove(pos, hex.node, None))
//            case list => list.map(n => RobberMove(pos, hex.node, Some(n)))
//          }
//      }
//    }.toList
//
//  implicit def canDoMove[BOARD <: BoardConfiguration, II <: Resource, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: RobberSOCStateImplicits[BOARD, II, PERSPECTIVE, STATE]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, RobberMove] =
//    (state, _, move) => {
//      import RobberSOCState._
//      state.canMoveRobber(move)
//    }
//}


//  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCRobberLocation :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[STATE]): ApplyMoveResult[RobberMoveResult[II], STATE] = {
//    (s, m) => s.moveRobber(m)
//  }




//trait InitialRobberLocationLocator[BOARD <: BoardConfiguration, STATE <: RobberSOCState[BOARD, _, _, STATE]] {
//  def getInitialRobberLocation(board: CatanBoard[BOARD]): Int
//}
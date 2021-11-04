package soc.moves2

import shapeless.{::, HList}
import shapeless.ops.hlist.SelectAll
import shapeless.ops.record.Updater
import soc.board.{BoardConfiguration, CatanBoard, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.inventory.resources.{ResourceSet, SOCTransactions, Steal}
import soc.moves.{MoveRobberAndStealMove, RobPlayer}
import soc.moves2.RobberSOCState.RobberSOCStateImplicits
import soc.moves2.SOCState._
import soc.moves2.build.{BoardOps, BuildSettlementMove, CityBoardOps, SOCCityMap, SOCSettlementMap, SettlementSOCState}
import soc.moves2.developmentcard.{BuyDevelopmentCardMove, BuyDevelopmentCardsMoveResult, DevelopmentCardInventoryHelper, SOCDevelopmentCardsInDeck}

case class RobberMove(player: Int, robberLocation: Int, playerStole: Option[Int]) extends SOCMove
case class RobberMoveResult[II <: InventoryItem](player: Int, robberLocation: Int, cardStole: Option[RobPlayer[II]]) extends SOCMoveResult[RobberMove] {
  override def move: RobberMove = RobberMove(player, robberLocation, cardStole.map(_.player))
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[RobberMove]] = playerIds.map {
    case `player` => player -> this
    case p if cardStole.fold(false)(_.player == p) => p -> this
    case p => p -> copy(cardStole = None)
  }.toMap
}

object RobberMove {

  implicit def generator[BOARD <: BoardConfiguration, II <: Resource, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: RobberSOCStateImplicits[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, RobberMove] =
    (state, _, pos) => {
      import SOCState._
      import RobberSOCState._
      implicit val (op1, boardOps) = ops
      import op1._

      val board = state.board
      board.hexesWithNodes.filterNot(_.node == state.robberLocation).flatMap { hex =>
        hex.vertices.flatMap(boardOps.vertexBuildingMap(state).get).map(_.playerId).distinct
          .filterNot(p => p == pos || state.playerInventories.get(p).fold(false)(_.numCards <= 0)) match {
            case Nil => List(RobberMove(pos, hex.node, None))
            case list => list.map(n => RobberMove(pos, hex.node, Some(n)))
          }
      }
    }.toList

  implicit def canDoMove[BOARD <: BoardConfiguration, II <: Resource, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: RobberSOCStateImplicits[BOARD, II, PERSPECTIVE, STATE]): CanDoMove[STATE[BOARD, II, PERSPECTIVE], PerfectInfo, RobberMove] =
    (state, _, move) => {
      import RobberSOCState._
      state.canMoveRobber(move)
    }
}

case class RobberAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: RobberSOCState[BOARD, II, P, STATE[P]]]()(implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, RobberMove, RobberMoveResult[II]]) extends GameAction[BOARD, II, STATE, RobberAction[BOARD, II, STATE]] {
  override type A = RobberMove
  override type R = RobberMoveResult[II]
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = true
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[RobberMove] = {
    val board = state.board
    board.hexesWithNodes.filterNot(_.node == board.robberHex).flatMap { hex =>
      board.playersOnHex(hex.node).filterNot(p => p == position || state.playerInventories.get(p).fold(false)(_.numCards <= 0)) match {
        case Nil => List(RobberMove(position, hex.node, None))
        case list => list.map(n => RobberMove(position, hex.node, Some(n)))
      }
    }
  }.toList
}

case class SOCRobberLocation(v: Int)

object RobberSOCState {

//  type RobberSOCStateImplicits[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], STATE[B, I, P] <: HList] = (SOCBaseOps1[B, I, P, STATE, SOCRobberLocation], BoardOps[STATE[B, I, P]])
//  implicit def getImplicits[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], STATE[B, I, P] <: HList](implicit baseOps: SOCBaseOps1[B, I, P, STATE, SOCRobberLocation], boardOps: BoardOps[STATE[B, I, P]]): RobberSOCStateImplicits[B, I, P, STATE] =
//    (baseOps, boardOps)

  implicit class RobberSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRobberLocation :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val robberLocation: SOCRobberLocation = dep.get(state)
    def updateRobberLocation(v: SOCRobberLocation): STATE = dep.update(v, state)

    def canMoveRobber(move: RobberMove): Boolean = {
      lazy val playersOnHex = state.board.hexesWithNodes.find(_.node == move.robberLocation).fold(List.empty[Int])(_.vertices.flatMap { v =>
        boardOps.vertexBuildingMap(state).get(v).map(_.playerId).toList
      }.distinct)
      boardOps.vertexBuildingMap(state).contains(Vertex(move.robberLocation)) &&
        move.robberLocation != robberLocation &&
        move.playerStole.fold(playersOnHex.isEmpty)(playersOnHex.contains)
    }

    def moveRobber(moveResult: RobberMoveResult[II]): STATE = {
      val RobberMoveResult(player, robberLocation: Int, steal: Option[RobPlayer[II]]) = moveResult
      updateRobberLocation(SOCRobberLocation(robberLocation))
        .updateTransactions(steal.fold(List.empty[SOCTransactions[II]])(s => List(Steal(player, s.player, s.res.map(r => CatanSet.fromList(Seq(r)))))))
    }
  }

  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCRobberLocation :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[STATE]): ApplyMoveResult[RobberMoveResult[II], STATE] = {
    (s, m) => s.moveRobber(m)
  }


}

//trait InitialRobberLocationLocator[BOARD <: BoardConfiguration, STATE <: RobberSOCState[BOARD, _, _, STATE]] {
//  def getInitialRobberLocation(board: CatanBoard[BOARD]): Int
//}
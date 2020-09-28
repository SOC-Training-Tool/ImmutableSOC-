package soc.moves2

import soc.board.{BoardConfiguration, CatanBoard, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, PerfectInfoInventory}
import soc.inventory.resources.{ResourceSet, SOCTransactions, Steal}
import soc.moves.RobPlayer
import soc.moves2.build.{CitySOCState, SettlementSOCState}

case class RobberMove(player: Int, robberLocation: Int, playerStole: Option[Int]) extends SOCMove[RobberMove]
case class RobberMoveResult[II <: InventoryItem](player: Int, robberLocation: Int, cardStole: Option[RobPlayer[II]]) extends SOCMoveResult[RobberMove] {
  override def move: RobberMove = RobberMove(player, robberLocation, cardStole.map(_.player))
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult[RobberMove]] = playerIds.map {
    case `player` => player -> this
    case p if cardStole.fold(false)(_.player == p) => p -> this
    case p => p -> copy(cardStole = None)
  }.toMap
}
case class RobberAction[BOARD <: BoardConfiguration, II <: InventoryItem, STATE[P] <: RobberSOCState[BOARD, II, P, STATE[P]]]()(implicit val moveResultProvider: MoveResultProvider[BOARD, II, STATE, RobberMove, RobberMoveResult[II]]) extends GameAction[BOARD, II, STATE, RobberMove] {
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
trait RobberSOCState[BOARD, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: RobberSOCState[BOARD, II, PERSPECTIVE, STATE]] extends SOCState[BOARD, II, PERSPECTIVE, STATE] {
  this: SettlementSOCState[BOARD, II, PERSPECTIVE, STATE] with CitySOCState[BOARD, II, PERSPECTIVE, STATE] =>
  def robberLocation: SOCRobberLocation
  def updateRobberLocation(v: SOCRobberLocation): STATE
  def canMoveRobber(move: RobberMove): Boolean = {
    val newRobberLocation = Vertex(move.robberLocation)
    board.vertices.contains(newRobberLocation) &&
      move.robberLocation != robberLocation &&
      move.playerStole.fold(true) { p =>
        board.hexesWithNodes.find(_.node == newRobberLocation.node).fold(List.empty[Int])(_.vertices.flatMap { v =>
          settlements.get(v).map(_.playerId).toList ::: cities.get(v).map(_.playerId).toList
        }.distinct).contains(p)
      }
  }

  def moveRobber(moveResult: RobberMoveResult[II]): STATE = {
    val RobberMoveResult(player, robberLocation: Int, steal: Option[RobPlayer[II]]) = moveResult
    updateRobberLocation(SOCRobberLocation(robberLocation))
      .updateTransactions(steal.fold(List.empty[SOCTransactions[II]])(s => List(Steal(player, s.player, s.res.map(r => CatanSet.fromList(Seq(r)))))))
  }
}

trait InitialRobberLocationLocator[BOARD <: BoardConfiguration, STATE <: RobberSOCState[BOARD, _, _, STATE]] {
  def getInitialRobberLocation(board: CatanBoard[BOARD]): Int
}
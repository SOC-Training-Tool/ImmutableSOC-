package soc.state

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem}
import soc.inventory.resources.{SOCTransactions, Steal}
import soc.moves2.{RobPlayer, RobberMove, RobberMoveResult}
import soc.state.SOCState.SOCState
import soc.state.build.BoardOps
import util.DependsOn

case class SOCRobberLocation(v: Int)

object RobberSOCState {

  implicit class RobberSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRobberLocation :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    import soc.state.SOCState._

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val robberLocation: SOCRobberLocation = dep.get(state)
    def updateRobberLocation(v: SOCRobberLocation): STATE = dep.update(v, state)

    def canMoveRobber(move: RobberMove[II]): Boolean = {
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
}

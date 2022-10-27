package soc.state.build

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, Settlement}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.build.BuildSettlementMove
import soc.state.{SOCPlayerPointsMap, SOCState}
import soc.state.SOCState.{SOCState, SOCStateOps}
import util.{DependsOn, MapWrapper}

case class SOCSettlementMap(m: Map[Vertex, Settlement]) extends MapWrapper[Vertex, Settlement]

trait SettlementBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildSettlement(buildSettlementMove: BuildSettlementMove, s: S)(f: S => S): S
}

object SettlementSOCState {

  implicit class SettlementSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val settlements: SOCSettlementMap = dep.get(state)

    def updateSettlements(settlements: Map[Vertex, Settlement]): STATE = dep.update(SOCSettlementMap(settlements), state)

    def buildSettlement(buildSettlementMove: BuildSettlementMove, buy: Option[CatanSet[II, Int]]): STATE = settlementBoardOps.onBuildSettlement(buildSettlementMove, state) { state =>
      val pointsForPlayer = state.playerPoints(buildSettlementMove.player)
      val us = state.updateSettlements(state.settlements + (buildSettlementMove.vertex -> Settlement(buildSettlementMove.player)))
      buy.fold(us) { cost =>
        us.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildSettlementMove.player, cost)))
      }.updatePoints(SOCPlayerPointsMap((state.playerPoints - buildSettlementMove.player) + (buildSettlementMove.player -> (pointsForPlayer + 1))))
    }

    def settlementsForPlayer(player: Int): Int = settlements.values.count(_.playerId == player)

    def canPlaceFreeSettlement(loc: Vertex): Boolean = {
      state.board.vertices.contains(loc) &&
        !settlementBoardOps.vertexBuildingMap(state).contains(loc) &&
        state.board.neighboringVertices(loc).forall { v => !settlementBoardOps.vertexBuildingMap(state).contains(v) }
    }

    def canBuildSettlement(buildSettlementMove: BuildSettlementMove): Boolean = {
      canPlaceFreeSettlement(buildSettlementMove.vertex) && {
        state.board.edgesFromVertex(buildSettlementMove.vertex).exists { edge =>
          settlementBoardOps.edgeBuildingMap(state).get(edge).fold(false)(_.playerId == buildSettlementMove.player)
        }
      }
    }
  }
}
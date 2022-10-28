package soc.base.actions

import shapeless.{::, HList, HNil}
import soc.base._
import soc.base.state._
import soc.board.{BoardConfiguration, Vertex}
import soc.core.Cost
import soc.core.SOCState._
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory._
import soc.state.build.BoardOps
import util.{DependsOn, UpdateState}

class BuildSettlementAction {

  trait SettlementBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
    def onBuildSettlement(buildSettlementMove: BuildSettlementMove, s: S)(f: S => S): S
  }

  implicit class SettlementOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
    implicit val settlementDep = dep.innerDependency[SOCSettlementMap :: HNil]

    def buildSettlement(buildSettlementMove: BuildSettlementMove, buy: Option[CatanSet[II, Int]]): STATE = {
      val pointsForPlayer = state.playerPoints.getOrElse(buildSettlementMove.player, 0)
      val us = state.updateSettlements(state.settlements + (buildSettlementMove.vertex -> Settlement(buildSettlementMove.player)))
      buy.fold(us) { cost =>
        us.updateTransactions(List(Gain(BANK_PLAYER_ID, cost), Lose(buildSettlementMove.player, cost)))
      }.updatePoints(SOCPlayerPointsMap(state.playerPoints + (buildSettlementMove.player -> (pointsForPlayer + 1))))
    }

    def canPlaceFreeSettlement(loc: Vertex): Boolean = {
      state.board.vertices.contains(loc) &&
        !boardOps.vertexBuildingMap(state).contains(loc) &&
        state.board.neighboringVertices(loc).forall { v => !boardOps.vertexBuildingMap(state).contains(v) }
    }

    def canBuildSettlement(buildSettlementMove: BuildSettlementMove): Boolean = {
      canPlaceFreeSettlement(buildSettlementMove.vertex) && {
        state.board.edgesFromVertex(buildSettlementMove.vertex).exists { edge =>
          boardOps.edgeBuildingMap(state).get(edge).fold(false)(_.playerId == buildSettlementMove.player)
        }
      }
    }
  }

  implicit val cost: Cost[Resource, BuildSettlementMove] = Cost.apply(ResourceSet.apply(Wood, Brick, Wheat, Sheep))

  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildSettlementAction]): UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] = new UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] {

    override def apply(t: STATE, u: BuildSettlementMove): STATE = {
      settlementBoardOps.onBuildSettlement(u, t)(_.buildSettlement(u, Some(cost.getCost)))
    }
  }
}



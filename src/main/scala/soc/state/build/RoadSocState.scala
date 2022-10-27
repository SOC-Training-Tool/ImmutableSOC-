package soc.state.build

import shapeless.{HList, :: => :::}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.{CatanSet, InventoryHelper, InventoryItem, Road}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.build.BuildRoadMove
import soc.state.{SOCPlayerPointsMap, SOCState}
import soc.state.SOCState.{SOCState, SOCStateOps}
import util.{DependsOn, MapWrapper}

case class SOCRoadMap(m: Map[Edge, Road]) extends MapWrapper[Edge, Road]

trait RoadBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildRoad(buildRoadMove: BuildRoadMove, s: S)(f: S => S): S
}

object RoadSOCState {

  implicit class RoadSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRoadMap ::: SOCState[BOARD, II, PERSPECTIVE]], roadBoardOps: RoadBoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val roads: SOCRoadMap = dep.get(state)
    def updateRoads(roads: SOCRoadMap): STATE = dep.update(roads, state)

    def buildRoad(buildRoadMove: BuildRoadMove, buy: Option[CatanSet[II, Int]]): STATE = roadBoardOps.onBuildRoad(buildRoadMove, state) { state =>
      val ur = updateRoads(SOCRoadMap(roads + (buildRoadMove.edge -> Road(buildRoadMove.player))))
      buy.fold(ur)(cost => ur.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildRoadMove.player, cost))))
    }

    def roadsForPlayer(player: Int): Int = roads.values.count(_.playerId == player)

    def canBuildRoad(buildRoadMove: BuildRoadMove): Boolean = {
      val loc = buildRoadMove.edge
      val playerId = buildRoadMove.player

      def canBuildRoadOffVertex(v: Vertex): Boolean = (v == loc.v1 || v == loc.v2) &&
        (roadBoardOps.vertexBuildingMap(state).get(v).fold(false)(_.playerId == playerId) ||
          (!roadBoardOps.vertexBuildingMap(state).get(v).fold(false)(_.playerId != playerId) &&
            state.board.edgesFromVertex(v).filterNot(_ == loc).exists { e => roads.get(e).fold(false)(_.playerId == playerId) }))

      state.board.edges.contains(loc) && !roadBoardOps.edgeBuildingMap(state).contains(loc) && (canBuildRoadOffVertex(loc.v1) || canBuildRoadOffVertex(loc.v2))
    }
  }
}

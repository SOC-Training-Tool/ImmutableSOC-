package soc.state.build

import shapeless.{HList, ::}
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.{EdgeBuilding, InventoryHelper, InventoryItem, VertexBuilding}
import soc.moves2.build._
import soc.state.build._
import scala.{:: => :#:}
import soc.state._

import soc.state.SOCState
import util.DependsOn

import scala.collection.immutable.{:: => :#:}

trait BoardOps[B, I, P, S] {
  def vertexBuildingMap(s: S): Map[Vertex, VertexBuilding]
  def edgeBuildingMap(s: S): Map[Edge, EdgeBuilding]
}

object BoardOps {

  import CitySOCState._
  import soc.state.LongestRoadSOCState._
  import RoadSOCState._
  import SOCState._
  import SettlementSOCState._

  implicit def baseBoardOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCCityMap :: SOCRoadMap :: SOCLongestRoadPlayer :: SOCRoadLengths :: SOCState[BOARD, II, PERSPECTIVE]]) =
    new BoardOps[BOARD, II, PERSPECTIVE, STATE]
      with SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]
      with CityBoardOps[BOARD, II, PERSPECTIVE, STATE]
      with RoadBoardOps[BOARD, II, PERSPECTIVE, STATE] { self =>

      implicit val boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE] with SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE] with CityBoardOps[BOARD, II, PERSPECTIVE, STATE] with RoadBoardOps[BOARD, II, PERSPECTIVE, STATE] = self

      private implicit val socStateDp = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      private implicit val settlementDp = dep.innerDependency[SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]]
      private implicit val roadDp = dep.innerDependency[SOCRoadMap :: SOCState[BOARD, II, PERSPECTIVE]]
      private implicit val cityDp = dep.innerDependency[SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]]
      private implicit val lrOp = dep.innerDependency[SOCLongestRoadPlayer :: SOCRoadLengths :: SOCState[BOARD, II, PERSPECTIVE]]

      override def vertexBuildingMap(s: STATE): Map[Vertex, VertexBuilding] = s.settlements ++ s.cities
      override def edgeBuildingMap(s: STATE): Map[Edge, EdgeBuilding] = s.roads.m

      override def onBuildSettlement(buildSettlementMove: BuildSettlementMove, s: STATE)(f: STATE => STATE): STATE = {
        // calculate if building settlement cuts road lengths and redistributes longest road
        val roadsOnVertex = s.board.edgesFromVertex(buildSettlementMove.vertex).flatMap(s.roads.get)
        val playerIds = roadsOnVertex.map(_.playerId).groupBy(identity).view.mapValues(_.length).toMap
        val otherPlayerIds = playerIds - buildSettlementMove.player
        otherPlayerIds.toList match {
          case (p, 2) :#: Nil => f(s).redistributeLongestRoad(p)
          case _ => f(s)
        }
      }
      override def onBuildCity(buildCity: BuildCityMove, s: STATE)(f: STATE => STATE): STATE = f(s.updateSettlements(s.settlements - buildCity.vertex))
      override def onBuildRoad(buildRoadMove: BuildRoadMove, s: STATE)(f: STATE => STATE): STATE = f(s).redistributeLongestRoad(buildRoadMove.player)
    }

}



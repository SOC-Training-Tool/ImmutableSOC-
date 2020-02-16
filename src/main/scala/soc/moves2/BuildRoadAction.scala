package soc.moves2

import soc.board.{CatanBoard, Edge, Vertex}
import soc.inventory.Road
import soc.inventory.resources.ResourceSet.Resources

case class BuildRoadMove(edge: Edge) extends SOCMove[BuildRoadMove]
case class BuildRoadAction(limit: Int, cost: Resources) extends EdgeBuildingAction[BuildRoadMove] {
  override type Build = Road

  override def canBuild(board: CatanBoard[_], loc: Edge, playerId: Int): Boolean = {
    def canBuildRoadOffVertex(v: Vertex): Boolean = {
      if (board.verticesBuildingMap.get(v).fold(false)(_.playerId == playerId)) true
      else if (board.verticesBuildingMap.get(v).fold(false)(_.playerId != playerId)) false
      else {
        board.edgesFromVertex(v).filterNot(_ == loc).exists { e =>
          board.edgesBuildingMap.contains(e) && board.edgesBuildingMap(e).playerId == playerId
        }
      }
    }
    board.edges.contains(loc) && !board.edgesBuildingMap.contains(loc) && (canBuildRoadOffVertex(loc.v1) || canBuildRoadOffVertex(loc.v2))
  }
}

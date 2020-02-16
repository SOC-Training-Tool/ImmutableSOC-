package soc.moves2

import soc.board.{CatanBoard, Vertex}
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.Settlement

case class BuildSettlementMove(vertex: Vertex) extends SOCMove[BuildSettlementMove]
case class BuildSettlementAction(limit: Int, cost: Resources) extends VertexBuildingAction[BuildSettlementMove] {

  override type Build = Settlement

  override def canBuild(board: CatanBoard[_], loc: Vertex, playerId: Int): Boolean = {
    canPlaceFreeSettlement(board, loc, playerId) &&  {
        board.edgesFromVertex(loc).exists { edge =>
          board.edgesBuildingMap.get(edge).fold(false)(_.playerId == playerId)
        }
      }
  }

  def canPlaceFreeSettlement(board: CatanBoard[_], loc: Vertex, playerId: Int): Boolean = {
    board.vertices.contains(loc) &&
      !board.verticesBuildingMap.contains(loc) &&
      board.neighboringVertices(loc).forall { v => !board.verticesBuildingMap.contains(v) }
  }
}
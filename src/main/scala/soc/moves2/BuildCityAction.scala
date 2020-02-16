package soc.moves2

import soc.board.{BoardConfiguration, CatanBoard, Vertex}
import soc.inventory.resources.ResourceSet.Resources
import soc.inventory.{City, Inventory, Settlement}
import soc.state.GameState

case class BuildCityMove(vertex: Vertex) extends SOCMove[BuildCityMove] {
  override def applyToSTate[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PERSPECTIVE, BOARD]): GameState[PERSPECTIVE, BOARD] = ???
}
case class BuildCityAction(limit: Int, cost: Resources) extends VertexBuildingAction[BuildCityMove] {

  override type Build = City
  override def canBuild(board: CatanBoard[_], vertex: Vertex, playerId: Int): Boolean = {
    board.verticesBuildingMap.contains(vertex) &&
      board.verticesBuildingMap(vertex).isInstanceOf[Settlement] && {
      val settlement = board.verticesBuildingMap(vertex)
      settlement.playerId == playerId
    }
  }
}





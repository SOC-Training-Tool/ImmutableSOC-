package soc.moves2

import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.Inventory
import soc.inventory.Inventory.PerfectInfo
import soc.state.{GamePhase, GameState}

case class InitialPlacementMove(vertex: Vertex, edge: Edge) extends SOCMove[InitialPlacementMove]
case class InitialPlacementAction(settlementAction: BuildSettlementAction, roadAction: BuildRoadAction) extends GameAction[InitialPlacementMove] {

  override def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): Seq[SOCMove[InitialPlacementMove]] = {
    if (state.phase == GamePhase.InitialPlacement) {
      settlementAction.getBoardLocations(state.board).filter(settlementAction.canPlaceFreeSettlement(state.board, _, state.currentPlayer)).flatMap { v: Vertex =>
        state.board.edgesFromVertex(v).filter(roadAction.canBuild(state.board, _, state.currentPlayer) ).map(e => InitialPlacementMove(v, e))
      }
    } else Nil
  }
}

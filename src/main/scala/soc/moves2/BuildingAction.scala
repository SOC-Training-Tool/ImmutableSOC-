package soc.moves2

import soc.board.{BoardConfiguration, BuildingLocation, CatanBoard, Edge, Vertex}
import soc.inventory.Inventory.PerfectInfo
import soc.inventory.{CatanBuilding, EdgeBuilding, Inventory, VertexBuilding}
import soc.inventory.resources.ResourceSet.Resources
import soc.state.{GamePhase, GameState}

import scala.reflect.ClassTag

trait BuildingAction[A <: SOCMove[A]] extends GameAction[A] {
  type BuildLoc <: BuildingLocation
  type Build <: CatanBuilding
  val cost: Resources
  val limit: Int

  def getAllPossibleMovesForState[PERSPECTIVE <: Inventory[PERSPECTIVE], BOARD <: BoardConfiguration](state: GameState[PerfectInfo, BOARD], inv: PERSPECTIVE, position: Int): List[SOCMove[A]] = {
    if (state.phase == GamePhase.BuyTradeOrEnd && (getAmountForPlayer(state.board, state.currentPlayer) < limit && inv.canSpend(cost))) {
      getPossibleLocations(state.board, state.currentPlayer).toList.map(v => apply(v))
    } else Nil
  }

  def getLocationsForPlayer(board: CatanBoard[_], id: Int)(implicit ct1: ClassTag[BuildLoc], ct2: ClassTag[Build]): Seq[BuildLoc] = board.buildingMap.toSeq.filter {
    case (_: BuildLoc, b: Build) if b.playerId == id => true
    case _ => false
  }.map(_._1.asInstanceOf[BuildLoc])
  def getAmountForPlayer(board: CatanBoard[_], id: Int): Int = getLocationsForPlayer(board, id).length

  def getPossibleLocations(board: CatanBoard[_], playerId: Int): Seq[BuildLoc] = getBoardLocations(board).filter(canBuild(board, _, playerId))

  def apply(location: BuildLoc): SOCMove[A]

  def canBuild(board: CatanBoard[_], loc: BuildLoc, playerId: Int): Boolean
  def getBoardLocations(board: CatanBoard[_]): Seq[BuildLoc]

}

trait VertexBuildingAction[A <: SOCMove[A]] extends BuildingAction[A] {
  override type BuildLoc = Vertex
  type Build <: VertexBuilding

  def getBoardLocations(board: CatanBoard[_]): Seq[BuildLoc] = board.vertices
}

trait EdgeBuildingAction[A <: SOCMove[A]] extends BuildingAction[A] {
  override type BuildLoc = Edge
  type Build <: EdgeBuilding

  def getBoardLocations(board: CatanBoard[_]): Seq[BuildLoc] = board.edges
}
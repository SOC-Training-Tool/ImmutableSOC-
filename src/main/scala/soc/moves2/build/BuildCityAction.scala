package soc.moves2.build

import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory.{CatanSet, City, InventoryHelper, InventoryItem, PerfectInfoInventory, Resource}
import soc.moves2.{PerfectInformationMoveGameAction, PerfectInformationSOCMove, SOCPlayerPointsMap, SOCState}
import util.MapWrapper

case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove[BuildCityMove]
case class BuildCityAction[BOARD <: BoardConfiguration, II <: Resource, STATE[P] <: CitySOCState[BOARD, P, II, STATE[P]]](limit: Int, cost: CatanSet[II, Int]) extends PerfectInformationMoveGameAction[BOARD, II, STATE, BuildCityMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    state.citiesForPlayer(position) < limit && inv.itemSet.contains(cost)
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[BuildCityMove] = {
    state.self.settlements.map(c => BuildCityMove(position, c._1)).filter(state.canBuildCity).toSeq
  }
}

case class SOCCityMap(m: Map[Vertex, City]) extends MapWrapper[Vertex, City]
trait CitySOCState[BOARD <: BoardConfiguration,  I <: InventoryItem, PERSPECTIVE <: InventoryHelper[I, PERSPECTIVE],STATE <: CitySOCState[BOARD, I, PERSPECTIVE, STATE]] extends SOCState[BOARD, I, PERSPECTIVE, STATE] {
  this: SettlementSOCState[BOARD, I, PERSPECTIVE, STATE] =>
  def self = this

  def cities: SOCCityMap

  def citiesForPlayer(player: Int): Int = cities.values.count(_.playerId == player)

  def updateCities(cities: SOCCityMap): STATE

  def canBuildCity(buildCityMove: BuildCityMove): Boolean = {
    settlements.get(buildCityMove.vertex).fold(false)(_.playerId == buildCityMove.player)
  }

  def buildCity(buildCityMove: BuildCityMove, cost: CatanSet[I, Int]): STATE = {
    val pointsForPlayer = playerPoints(buildCityMove.player)
    updateCities(SOCCityMap(cities + (buildCityMove.vertex -> City(buildCityMove.player)))).self
      .updateSettlements(SOCSettlementMap(settlements - buildCityMove.vertex)).self
      .updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildCityMove.player, cost)))
      .updatePoints(SOCPlayerPointsMap((playerPoints - buildCityMove.player) + (buildCityMove.player -> (pointsForPlayer + 1))))

  }
}
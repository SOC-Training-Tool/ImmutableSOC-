package soc.state.build

import shapeless.{::, HList}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.{CatanSet, City, InventoryHelper, InventoryItem, Settlement}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.build.BuildCityMove
import soc.state.{SOCPlayerPointsMap, SOCState}
import soc.state.SOCState._
import util.{DependsOn, MapWrapper}

case class SOCCityMap(m: Map[Vertex, City]) extends MapWrapper[Vertex, City]

trait CityBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildCity(buildCity: BuildCityMove, s: S)(f: S => S): S
}

object CitySOCState {

  implicit class CitySOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val cities: SOCCityMap = dep.get(state)

    def updateCities(cities: SOCCityMap): STATE = dep.update(cities, state)

    def buildCity(buildCityMove: BuildCityMove, cost: CatanSet[II, Int]): STATE = cityBoardOps.onBuildCity(buildCityMove, state) { state =>
      val pointsForPlayer = state.playerPoints(buildCityMove.player)
      updateCities(SOCCityMap(cities + (buildCityMove.vertex -> City(buildCityMove.player))))
        .updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildCityMove.player, cost)))
        .updatePoints(SOCPlayerPointsMap((state.playerPoints - buildCityMove.player) + (buildCityMove.player -> (pointsForPlayer + 1))))
    }

    def citiesForPlayer(player: Int): Int = cities.values.count(_.playerId == player)

    def canBuildCity(buildCityMove: BuildCityMove): Boolean = {
      cityBoardOps.vertexBuildingMap(state).get(buildCityMove.vertex).fold(false) {
        case s: Settlement => s.playerId == buildCityMove.player
        case _ => false
      }
    }
  }
}
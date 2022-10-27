package soc.state.build

import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.{CatanSet, City, InventoryHelper, InventoryItem, Settlement}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.Cost
import soc.moves2.build.BuildCityMove
import soc.state.{SOCPlayerPointsMap, SOCState, UpdateState}
import soc.state.SOCState._
import util.{DependsOn, MapWrapper}

case class SOCCityMap(m: Map[Vertex, City]) extends MapWrapper[Vertex, City]

trait CityBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildCity(buildCity: BuildCityMove, s: S)(f: S => S): S
}

object CitySOCState {

  implicit class CitySOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCityMap :: HNil]) {

    val cities: SOCCityMap = dep.get(state)

    def updateCities(cities: SOCCityMap): STATE = dep.update(cities, state)
  }


//  implicit class CitySOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], boardOps: BoardOps[BOARD, II, PERSPECTIVE, STATE]) {
//
//    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
//
//    def buildCity(buildCityMove: BuildCityMove, cost: CatanSet[II, Int]): STATE = {
//      val pointsForPlayer = state.playerPoints(buildCityMove.player)
//      updateCities(SOCCityMap(cities + (buildCityMove.vertex -> City(buildCityMove.player))))
//        .updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildCityMove.player, cost)))
//        .updatePoints(SOCPlayerPointsMap((state.playerPoints - buildCityMove.player) + (buildCityMove.player -> (pointsForPlayer + 1))))
//    }
//
//    def citiesForPlayer(player: Int): Int = cities.values.count(_.playerId == player)
//
//    def canBuildCity(buildCityMove: BuildCityMove): Boolean = {
//      boardOps.vertexBuildingMap(state).get(buildCityMove.vertex).fold(false) {
//        case s: Settlement => s.playerId == buildCityMove.player
//        case _ => false
//      }
//    }
//  }

  implicit def updateState[B <: BoardConfiguration, I <: InventoryItem, P <: InventoryHelper[I, P], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[B, I, P]], cityBoardOps: CityBoardOps[B, I, P, STATE], cost: Cost[I, BuildCityMove]): UpdateState[B, I, P, BuildCityMove, STATE] = new UpdateState[B, I, P, BuildCityMove, STATE] {

    override def apply(t: STATE, u: BuildCityMove): STATE =
      cityBoardOps.onBuildCity(u, t)(_.buildCity(u, cost.getCost))
  }
}
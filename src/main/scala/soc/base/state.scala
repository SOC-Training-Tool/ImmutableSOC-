package soc.base

import shapeless.{::, HList, HNil}
import soc.board.{Edge, Vertex}
import soc.inventory.{City, Road, Settlement}
import util.{DependsOn, MapWrapper}

case class SOCTurn(t: Int)
case class SOCPlayerPointsMap(m: Map[Int, Int]) extends MapWrapper[Int, Int]

case class SOCCanRollDice(b: Boolean)

case class SOCPlayersToDiscard(players: List[Int])
case class SOCRobberLocation(v: Int)

case class SOCSettlementMap(m: Map[Vertex, Settlement]) extends MapWrapper[Vertex, Settlement]
case class SOCRoadMap(m: Map[Edge, Road]) extends MapWrapper[Edge, Road]
case class SOCCityMap(m: Map[Vertex, City]) extends MapWrapper[Vertex, City]

case class SOCLongestRoadPlayer(player: Option[Int])
case class SOCRoadLengths(m: Map[Int, Int]) extends MapWrapper[Int, Int]

object state {

  implicit class SettlementSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: HNil]) {
    val settlements: SOCSettlementMap = dep.get(state)
    def updateSettlements(settlements: Map[Vertex, Settlement]): STATE = dep.update(SOCSettlementMap(settlements), state)
    def settlementsForPlayer(player: Int): Int = settlements.values.count(_.playerId == player)
  }

  implicit class RoadSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRoadMap :: HNil]) {
    val roads: SOCRoadMap = dep.get(state)
    def updateRoads(roads: SOCRoadMap): STATE = dep.update(roads, state)
    def roadsForPlayer(player: Int): Int = roads.values.count(_.playerId == player)
  }

  implicit class CitySOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCityMap :: HNil]) {
    val cities: SOCCityMap = dep.get(state)
    def updateCities(cities: SOCCityMap): STATE = dep.update(cities, state)
    def citiesForPlayer(player: Int): Int = cities.values.count(_.playerId == player)
  }

  implicit class RollDiceSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCCanRollDice :: HNil]) {
    val rolledDice = dep.get[SOCCanRollDice](state).b
    def setRollDice(b: Boolean): STATE = dep.update(SOCCanRollDice(b), state)
  }

  implicit class RobberSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCRobberLocation :: HNil]) {
    val robberLocation: SOCRobberLocation = dep.get(state)
    def updateRobberLocation(v: SOCRobberLocation): STATE = dep.update(v, state)
  }

  implicit class SOCDiscardStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCPlayersToDiscard :: HNil]) {
    val playersToDiscard: SOCPlayersToDiscard = dep.get(state)
    def updatePlayersToDiscard(playersToDiscard: SOCPlayersToDiscard): STATE = dep.update(playersToDiscard, state)
  }

  implicit class SOCLongestRoadSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCLongestRoadPlayer :: SOCRoadLengths :: HNil]) {
    val longestRoadPlayer: SOCLongestRoadPlayer = dep.get(state)
    val roadLengths: SOCRoadLengths = dep.get(state)
    def updateLongestToadPlayer(longestRoadPlayer: SOCLongestRoadPlayer): STATE = dep.update(longestRoadPlayer, state)
    def updateRoadLengths(roadLengths: SOCRoadLengths): STATE = dep.update(roadLengths, state)
    def roadLengthForPlayer(player: Int): Int = roadLengths.get(player).getOrElse(0)
  }
}
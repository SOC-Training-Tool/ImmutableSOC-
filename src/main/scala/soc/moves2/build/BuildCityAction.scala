package soc.moves2.build

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory._
import soc.moves2.SOCState._
import soc.moves2.{SOCState, SOCState => _, _}
import util.MapWrapper

case class BuildCityMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove

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

  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
    (state: STATE, _: PerfectInfo, pos: Int) =>
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      state.board.vertices.map(BuildCityMove(pos, _)).filter(state.canBuildCity)
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildCityMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
    (state, inv, player) =>
      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      implicit val cityDep = dep.innerDependency[SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]]
      implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]
      state.rolledDice && state.currentPlayer == player && state.citiesForPlayer(player) < 4 && inv.canSpend(cost.getCost)
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildCityMove] = {
    (state, inv, move) =>
      canDoAction(state, inv, move.player) && state.canBuildCity(move)
  }

  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCCityMap :: SOCState[BOARD, II, PERSPECTIVE]], cityBoardOps: CityBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildCityMove]): ApplyMoveResult[BOARD, II, PERSPECTIVE, BuildCityMove, STATE] = (s, m) =>
    s.buildCity(m, cost.getCost)

  type BASE_NEXT_MOVES[W[_ <: SOCMoveResult]] = W[BuildSettlementMove] :: W[BuildRoadMove] :: W[BuildCityMove] :: W[EndTurnMove] :: HNil // TODO add full list
  implicit def baseNextMoves[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], W[_ <: SOCMoveResult], A <: HList, STATE <: HList](implicit ws: Selector[A, W[BuildCityMove]], sa: SelectAll[A, BASE_NEXT_MOVES[W]]): NextMove[BOARD, II, PERSPECTIVE, W, A, STATE, BuildCityMove] = (a: A) => {
    val func = (_: STATE, r: BuildCityMove) => Map(r.player -> sa.apply(a).toList)
    ws.apply(a) -> func
  }
}

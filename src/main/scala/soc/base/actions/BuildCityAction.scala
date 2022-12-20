package soc.base.actions

import shapeless.{::, HList, HNil}
import soc.base._
import soc.base.state._
import soc.board.BoardConfiguration
import soc.core.Cost
import soc.core.SOCState._
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory.{CatanSet, City, InventoryHelper, InventoryItem, Ore, Resource, Settlement, Wheat}
import util.{DependsOn, UpdateState}
import zio.ZIO

object BuildCityAction {

  implicit val cost: Cost[Resource, BuildCityMove] = Cost.apply(ResourceSet.apply(Ore, Ore, Ore, Wheat, Wheat))

  implicit def updateState[
    BOARD <: BoardConfiguration,
    II <: InventoryItem,
    PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE],
    STATE <: HList]
  (implicit dep: DependsOn[STATE, SOCCityMap :: SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]],
   cost: Cost[II, BuildCityMove])
  : UpdateState[BOARD, II, PERSPECTIVE, BuildCityMove, STATE] =
    new UpdateState[BOARD, II, PERSPECTIVE, BuildCityMove, STATE] {
    override def apply(state: STATE, buildCityMove: BuildCityMove): STATE = {

      implicit val cityDep = dep.innerDependency[SOCCityMap :: HNil]
      implicit val settlementDep = dep.innerDependency[SOCSettlementMap :: HNil]
      implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

      val pointsForPlayer = state.playerPoints(buildCityMove.player)
      state
        .updateSettlements(state.settlements - buildCityMove.vertex)
        .updateCities(SOCCityMap(state.cities + (buildCityMove.vertex -> City(buildCityMove.player))))
        .updateTransactions(List(Gain(BANK_PLAYER_ID, cost.getCost), Lose(buildCityMove.player, cost.getCost)))
        .updatePoints(SOCPlayerPointsMap((state.playerPoints - buildCityMove.player) + (buildCityMove.player -> (pointsForPlayer + 1))))
    }
  }
}

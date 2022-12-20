package soc.base.actions

import shapeless.{::, HList, HNil}
import soc.base._
import soc.base.state._
import soc.board.{BoardConfiguration, Vertex}
import soc.core.Cost
import soc.core.SOCState._
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory._
import util.{DependsOn, UpdateState}

object BuildSettlementAction {

  implicit val cost: Cost[Resource, BuildSettlementMove] = Cost.apply(ResourceSet.apply(Wood, Brick, Wheat, Sheep))

  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], cost: Cost[II, BuildSettlementMove]): UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] = new UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] {

    override def apply(state: STATE, buildSettlementMove: BuildSettlementMove): STATE = {

      implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      implicit val settlementDep = dep.innerDependency[SOCSettlementMap :: HNil]

      val pointsForPlayer = state.playerPoints.getOrElse(buildSettlementMove.player, 0)
      state
        .updateSettlements(state.settlements + (buildSettlementMove.vertex -> Settlement(buildSettlementMove.player)))
        .updateTransactions(List(Gain(BANK_PLAYER_ID, cost.getCost), Lose(buildSettlementMove.player, cost.getCost)))
        .updatePoints(SOCPlayerPointsMap(state.playerPoints + (buildSettlementMove.player -> (pointsForPlayer + 1))))
    }
  }
}



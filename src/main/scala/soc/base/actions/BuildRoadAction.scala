package soc.base.actions

import shapeless.{:+:, CNil, Coproduct, HList}
import soc.base.{BuildRoadMove, BuildSettlementMove, SOCRoadMap}
import soc.board.BoardConfiguration
import soc.core.Cost
import soc.core.SOCState.SOCState
import soc.inventory.{Brick, InventoryHelper, InventoryItem, Resource, Wood}
import soc.inventory.resources.ResourceSet
import util.{DependsOn, UpdateState}

object BuildRoadAction {


  implicit val cost: Cost[Resource, BuildRoadMove] = Cost.apply(ResourceSet.apply(Wood, Brick))

  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCRoadMap :: SOCState[BOARD, II, PERSPECTIVE]], cost: Cost[II, BuildRoadMove]): UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] = new UpdateState[BOARD, II, PERSPECTIVE, BuildRoadMove, STATE] {
    override def apply(t: STATE, u: BuildRoadMove): STATE = ???
  }
}
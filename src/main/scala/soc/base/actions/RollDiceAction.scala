package soc.base.actions

import game.GameAction
import shapeless.ops.coproduct
import shapeless.{::, Coproduct, HNil}
import soc.base.RollDiceMoveResult
import soc.base.state.ops._
import soc.base.state.{Bank, RobberLocation, VertexBuildingState}
import soc.inventory.ResourceInventories
import soc.inventory.Transactions.PerfectInfo
import util.DependsOn

object RollDiceAction {

  def apply[II, VB <: Coproduct, BOARD, INV[_]]
  (implicit inv: ResourceInventories[II, PerfectInfo[II], INV],
   socBoard: SOCBoard[II, BOARD],
   vertexFolder: coproduct.Folder.Aux[ResourcesForBuildingPoly.type, VB, Int]
  ): GameAction[RollDiceMoveResult, RobberLocation :: VertexBuildingState[VB] :: BOARD :: Bank[II] :: INV[II] :: HNil] = {
    GameAction[RollDiceMoveResult, RobberLocation :: VertexBuildingState[VB] :: BOARD :: Bank[II] :: INV[II] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[RobberLocation :: VertexBuildingState[VB] :: BOARD :: Bank[II] :: INV[II] :: HNil]
      state.distributeResources(state.getResourcesGainedOnRoll(move.result))
    }
  }

}

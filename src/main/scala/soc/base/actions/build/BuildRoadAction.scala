package soc.base.actions.build

import game.{GameAction, InventorySet}
import shapeless.{:+:, ::, CNil, Coproduct, HNil}
import soc.base.state.ops._
import soc.core.Transactions.PerfectInfo
import soc.core.state.ops.BankInvOps
import soc.core.state.{Bank, EdgeBuildingState}
import soc.core.{BuildRoadMove, ResourceInventories, Road}
import util.DependsOn
import util.opext.Embedder

object BuildRoadAction {

  def apply[Res, Inv[_], EB <: Coproduct]
  (cost: InventorySet[Res, Int])
  (implicit inv: ResourceInventories[Res, PerfectInfo[Res], Inv],
   roadEmbedder: Embedder[EB, Road.type :+: CNil],
  ): GameAction[BuildRoadMove, EdgeBuildingState[EB] :: Bank[Res] :: Inv[Res] :: HNil] = {
    GameAction[BuildRoadMove, EdgeBuildingState[EB] :: Bank[Res] :: Inv[Res] :: HNil] { case (move, state) =>
      implicit val roadDep =
        DependsOn[EdgeBuildingState[EB] :: Bank[Res] :: Inv[Res] :: HNil, EdgeBuildingState[EB] :: HNil]
      implicit val invDep =
        DependsOn[EdgeBuildingState[EB] :: Bank[Res] :: Inv[Res] :: HNil, Bank[Res] :: Inv[Res] :: HNil]
      state.addRoad(move.edge, move.player).payToBank(move.player, cost)
    }
  }
}
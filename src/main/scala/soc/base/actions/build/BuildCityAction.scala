package soc.base.actions.build

import game.{GameAction, InventorySet}
import shapeless.{:+:, ::, CNil, Coproduct, HNil}
import soc.base.BuildCityMove
import soc.base.state.ops._
import soc.base.state.{Bank, PlayerPoints, VertexBuildingState}
import soc.core.{City, Settlement}
import soc.inventory.ResourceInventories
import soc.inventory.Transactions.PerfectInfo
import util.DependsOn
import util.opext.Embedder

object BuildCityAction {

  def apply[Res, Inv[_], VB <: Coproduct]
  (cost: InventorySet[Res, Int])
  (implicit inv: ResourceInventories[Res, PerfectInfo[Res], Inv],
   cityEmbedder: Embedder[VB, City.type :+: Settlement.type :+: CNil]
  ): GameAction[BuildCityMove, VertexBuildingState[VB] :: PlayerPoints :: Bank[Res] :: Inv[Res] :: HNil] = {
    GameAction[BuildCityMove, VertexBuildingState[VB] :: PlayerPoints :: Bank[Res] :: Inv[Res] :: HNil] { case (move, state) =>
      val dep = DependsOn.single[VertexBuildingState[VB] :: PlayerPoints :: Bank[Res] :: Inv[Res] :: HNil]
      implicit val cityDep = dep.innerDependency[VertexBuildingState[VB] :: PlayerPoints :: HNil]
      implicit val invDep = dep.innerDependency[Bank[Res] :: Inv[Res] :: HNil]
      state.buildCity(move.vertex, move.player).payToBank(move.player, cost)
    }
  }
}

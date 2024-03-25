package soc.base.actions.developmentcards

import game.GameAction
import shapeless.ops.coproduct
import shapeless.{:+:, ::, CNil, Coproduct, HNil}
import soc.base.PlayRoadBuilderMove
import soc.base.state.ops.BuildRoadStateOps
import soc.core.state.{EdgeBuildingState, Turn}
import soc.core.{DevelopmentCardInventories, Road}
import util.DependsOn
import util.opext.Embedder

case object RoadBuilder

object PlayRoadBuilderAction {

  def apply[Dev <: Coproduct, DevInv[_], EB <: Coproduct]
  (implicit dev: DevelopmentCardInventories[Dev, DevInv],
   inject: coproduct.Inject[Dev, RoadBuilder.type],
   roadEmbedder: Embedder[EB, Road.type :+: CNil]
  ): GameAction[PlayRoadBuilderMove, DevInv[Dev] :: Turn :: EdgeBuildingState[EB] :: HNil] = {
    GameAction[PlayRoadBuilderMove, EdgeBuildingState[EB] :: HNil] { case (move, state) =>
      implicit val dep = DependsOn.single[EdgeBuildingState[EB] :: HNil]
      val result = state.addRoad(move.edge1, move.player)
      move.edge2.fold(result)(e => result.addRoad(e, move.player))
    }.extend(PlayDevelopmentCardActionExtension[PlayRoadBuilderMove, Dev, RoadBuilder.type, DevInv](RoadBuilder))
  }
}

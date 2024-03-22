package soc.base.actions.special

import game.ActionExtension
import shapeless.{::, Coproduct, HNil}
import soc.base.actions.SOCBoard
import soc.base.state.ops._
import soc.base.state.{EdgeBuildingState, LongestRoadOps, PlayerPoints, SOCLongestRoadPlayer, SOCRoadLengths, VertexBuildingState}
import util.DependsOn

object LongestRoadExtension {

  def apply[M, Res, VB <: Coproduct, EB <: Coproduct, BOARD]
  (implicit socBoard: SOCBoard[Res, BOARD]): ActionExtension[M, SOCRoadLengths :: SOCLongestRoadPlayer :: BOARD :: VertexBuildingState[VB] :: EdgeBuildingState[EB] :: PlayerPoints :: HNil] = {
    new ActionExtension[M, SOCRoadLengths :: SOCLongestRoadPlayer :: BOARD :: VertexBuildingState[VB] :: EdgeBuildingState[EB] :: PlayerPoints :: HNil] {
      override def apply(move: M, pre: STATE, post: STATE): STATE = {
        implicit val dep = DependsOn.single[STATE]
        implicit val pointDep = dep.innerDependency[PlayerPoints :: HNil]

        val board = post.select[BOARD]
        val edgeBuildingMap = post.select[EdgeBuildingState[EB]]
        val vertexBuildingMap = post.select[VertexBuildingState[VB]]
        val roadLengthOps = new LongestRoadOps(board, edgeBuildingMap, vertexBuildingMap)

        val longestRoadPlayer = pre.longestRoadPlayer
        val updatedRoadLengths = roadLengthOps.calcLongestRoadLengths()
        val updatedLongestRoadPlayer = updatedSpecialPlayer(5, longestRoadPlayer, updatedRoadLengths.m)
        updateState[STATE](longestRoadPlayer, updatedLongestRoadPlayer, post)(
          _.incrementPointForPlayer(_),
          _.decrementPointForPlayer(_),
          _.updateLongestRoadPlayer(_))
      }
    }
  }

}



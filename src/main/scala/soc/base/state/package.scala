package soc.base

import game.{InventorySet, StateInitializer}
import shapeless.Coproduct
import soc.board.{Edge, Vertex}

package object state {

  case class PlayerIds(players: Seq[Int])

  case class PlayerBuilding[BB <: Coproduct](building: BB, player: Int)

  type BoardBuildingState[BB <: Coproduct, T] = Map[T, PlayerBuilding[BB]]
  type VertexBuildingState[BB <: Coproduct] = BoardBuildingState[BB, Vertex]
  type EdgeBuildingState[BB <: Coproduct] = BoardBuildingState[BB, Edge]

  implicit def initBoardBuildingState[BB <: Coproduct, T]: StateInitializer[BoardBuildingState[BB, T]] = new StateInitializer[BoardBuildingState[BB, T]] {
    override def apply(): BoardBuildingState[BB, T] = Map.empty
  }

  case class Bank[II](b: InventorySet[II, Int])

  case class Turn(t: Int)

  object Turn {
    implicit val initTurnStat: StateInitializer[Turn] = new StateInitializer[Turn] {
      override def apply(): Turn = Turn(0)
    }
  }

  case class PlayerPoints(points: Map[Int, Int])

  object PlayerPoints {
    implicit def initPlayerPoints(implicit ids: PlayerIds): StateInitializer[PlayerPoints] = new StateInitializer[PlayerPoints] {
      override def apply(): PlayerPoints = PlayerPoints(ids.players.map(_ -> 0).toMap)
    }
  }

  case class RobberLocation(robberHexId: Int)

  case class SOCLongestRoadPlayer(player: Option[Int])

  object SOCLongestRoadPlayer {
    implicit val initLongestRoadPlayer = new StateInitializer[SOCLongestRoadPlayer] {
      override def apply(): SOCLongestRoadPlayer = SOCLongestRoadPlayer(None)
    }
  }

  case class SOCRoadLengths(m: Map[Int, Int])

  object SOCRoadLengths {
    implicit def initRoadCount(implicit playerIds: PlayerIds) = new StateInitializer[SOCRoadLengths] {
      override def apply(): SOCRoadLengths = SOCRoadLengths(playerIds.players.map(_ -> 0).toMap)
    }
  }

  case class LargestArmyPlayer(player: Option[Int])

  object LargestArmyPlayer {
    implicit val initLargestArmyPlayer = new StateInitializer[LargestArmyPlayer] {
      override def apply(): LargestArmyPlayer = LargestArmyPlayer(None)
    }
  }

  case class PlayerArmyCount(m: Map[Int, Int])

  object PlayerArmyCount {
    implicit def initArmyCount(implicit playerIds: PlayerIds) = new StateInitializer[PlayerArmyCount] {
      override def apply(): PlayerArmyCount = PlayerArmyCount(playerIds.players.map(_ -> 0).toMap)
    }
  }
}

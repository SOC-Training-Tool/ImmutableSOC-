package soc.base

import game.{InventorySet, StateInitializer}
import shapeless.Coproduct
import soc.core.state.PlayerIds
import soc.core.{Edge, Vertex}

package object state {

  case class RobberLocation(robberHexId: Int)

  case class DevelopmentCardDeckSize(size: Int)

  case class DevelopmentCardDeck[Dev](cards: List[Dev])

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

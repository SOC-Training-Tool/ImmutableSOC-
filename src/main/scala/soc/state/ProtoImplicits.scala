package soc.state

import soc.inventory.developmentCard.DevelopmentCardSet._
import protos.soc.state.{Player, PublicGameState, PublicPlayerState => PPState}
import soc.inventory.Inventory.NoInfo
import soc.inventory.{Inventory, Knight, NoInfoInventoryHelper}
import soc.proto.ProtoCoder
import soc.proto.ProtoCoder.ops._
import soc.state.player.{PlayerState, PlayerStateHelper}
import soc.inventory.ProtoImplicits._
import soc.board.ProtoImplicits._
import soc.board.BaseCatanBoard.baseBoardMapping
import soc.core.GameRules
import soc.inventory.resources.CatanResourceSet

object ProtoImplicits {

  implicit def protoPublicPlayerState[T <: Inventory[T]]: ProtoCoder[PlayerStateHelper[T], Map[Int, PPState]] = { ps =>
    ps.players.map { case (position, player) =>
      position -> PPState(
        Player(player.name, position),
        player.inventory.toPublicInfo.proto,
        player.roadLength,
        player.playedDevCards.getAmount(Knight),
        player.armyPoints == 2,
        player.roadPoints == 2
      )
    }
  }
  implicit val publicGameStateFromProto: ProtoCoder[PublicGameState, GameState[NoInfo]] = { gs =>

    // Todo embed and then extract game rules into PublicGameState
    implicit val gameRules = GameRules.default

    implicit val invHelper = NoInfoInventoryHelper()
    val helper = {
      val board = gs.board.proto
      val playerStates = gs.playerStates.map[Int, PlayerState[NoInfo]] { case pos -> ps =>
        pos -> PlayerState[NoInfo](
          ps.player.name,
          ps.player.position,
          ps.publicInventory.proto,
          if (ps.hasLargest) 2 else 0,
          if (ps.hasLongest) 2 else 0,
          board.getPortsForPlayer(pos),
          board.getSettlementVerticesForPlayer(pos).toList,
          board.getNumCityVerticesForPlayer(pos).toList,
          board.getRoadEdgesForPlayer(pos).toList,
          CatanResourceSet.empty[Int],
          board.longestRoadLength(pos)
        )
      }
      PlayerStateHelper(playerStates)
    }
    GameState(gs, helper)
  }

}

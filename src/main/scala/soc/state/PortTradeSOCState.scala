package soc.state

import shapeless.HList
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.{InventoryHelper, Misc, Port, Resource}
import soc.inventory.resources.{Gain, Lose}
import soc.moves2.PortTradeMove
import soc.state.SOCState.SOCState
import soc.state.build.BoardOps
import util.DependsOn

object PortTradeSOCState {

  implicit class PortTradeSOCStateOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCState[BOARD, Resource, PERSPECTIVE]], boardOps: BoardOps[BOARD, Resource, PERSPECTIVE, STATE]) {

    import soc.state.SOCState._

    def getPortsForPlayer(playerId: Int): Seq[Port] = state.board.portMap.filter { case (edge, _) =>
      def hasPortOnVertex(v: Vertex) =  boardOps.vertexBuildingMap(state).get(v).fold(false)(_.playerId == playerId)
      hasPortOnVertex(edge.v1) || hasPortOnVertex(edge.v2)
    }.map(_._2).toSeq

    def canPortTrade(portTradeMove: PortTradeMove): Boolean = {
      val ports = getPortsForPlayer(portTradeMove.player)
      portTradeMove.get.getTotal == 1 && portTradeMove. give.getTypes.length == 1 && {
        (portTradeMove.give, portTradeMove.give.getTotal) match {
          case (give, 2) if ports.contains(give.getTypes.head) => true
          case (_, 3) if ports.contains(Misc) => true
          case (_, 4) => true
          case _ => false
        }
      }
    }

    def doPortTrade(portTradeMove: PortTradeMove): STATE = state.updateTransactions(List(
      Gain(portTradeMove.player, portTradeMove.get),
      Gain(BANK_PLAYER_ID, portTradeMove.give),
      Lose(portTradeMove.player, portTradeMove.give),
      Lose(BANK_PLAYER_ID, portTradeMove.get),
    ))
  }
}

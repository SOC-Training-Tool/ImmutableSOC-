package soc.moves2

import soc.board.{BoardConfiguration, Edge}
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory.{CatanSet, Inventory, InventoryHelper, InventoryItem, Misc, PerfectInfoInventory, Port, Resource}
import soc.inventory.resources.ResourceSet.Resources
import soc.moves2.build.{CitySOCState, SettlementSOCState}
import soc.state.GameState

case class PortTradeMove(player: Int, give: Resources, get: Resources) extends PerfectInformationSOCMove[PortTradeMove]
case class PortTradeAction[BOARD <: BoardConfiguration, STATE[P] <: PortSOCState[BOARD, P, STATE[P]]]() extends PerfectInformationMoveGameAction[BOARD, Resource, STATE, PortTradeMove] {
  override def canDoAction[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
    inv.numCards >= 2 && state.getPortsForPlayer(position).length > 0
  }
  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PortTradeMove] = {
    def canSpend(res: Resource, amount: Int): Boolean = inv.canSpend(CatanSet.fromMap[Resource, Int](Map(res -> amount)))
    def generate(res: Resource, otherRes: Seq[Resource], amount: Int) = {
      val give = CatanSet.fromMap(Map(res -> amount))
      otherRes.map { r =>
        val get = CatanSet.fromMap(Map(r -> 1))
        PortTradeMove(position, give, get)
      }
    }
    val ports = state.getPortsForPlayer(position)
    val _3to1 = ports.contains(Misc)
    inv.itemSet.getTypes.flatMap { res =>
      val otherRes: Seq[Resource] = inv.itemSet.getTypes.filterNot(_ == res)
      if (ports.contains(res) && canSpend(res, 2)) generate(res, otherRes, 2)
      else if (_3to1 && canSpend(res, 3)) generate(res, otherRes, 3)
      else if (canSpend(res, 4)) generate(res, otherRes, 4)
      else Nil
    }.filter(state.canPortTrade)
  }
}

trait PortSOCState[BOARD, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE <: PortSOCState[BOARD, PERSPECTIVE, STATE]] extends SOCState[BOARD, Resource, PERSPECTIVE, STATE] {
  this: SettlementSOCState[BOARD, Resource, PERSPECTIVE, STATE] with CitySOCState[BOARD, Resource, PERSPECTIVE, STATE] =>

  def getPortsForPlayer(playerId: Int): Seq[Port] = board.portMap.filter { case (edge, _) =>
    settlements.get(edge.v1).fold(false)(_.playerId == playerId) ||  settlements.get(edge.v2).fold(false)(_.playerId == playerId) ||
      cities.get(edge.v1).fold(false)(_.playerId == playerId) ||  cities.get(edge.v2).fold(false)(_.playerId == playerId)
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

  def doPortTrade(portTradeMove: PortTradeMove): STATE = updateTransactions(List(
    Lose(portTradeMove.player, portTradeMove.give),
    Gain(portTradeMove.player, portTradeMove.get),
    Gain(SOCState.BANK_PLAYER_ID, portTradeMove.give),
    Lose(SOCState.BANK_PLAYER_ID, portTradeMove.get),
  ))

}

package soc.moves2

import shapeless.HList
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.InventoryHelper.{PerfectInvHelper, perfectInfoInvFactory}
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory.{CatanSet, Inventory, InventoryHelper, InventoryItem, Misc, PerfectInfoInventory, Port, Resource}
import soc.inventory.resources.ResourceSet.Resources
import soc.moves2.PortSOCState.PortSOCStateImplicit
import soc.moves2.SOCState.SOCState
import soc.moves2.SOCState.SOCBaseOps0
import soc.moves2.build.{BoardOps, CitySOCState, SettlementSOCState}

case class PortTradeMove(player: Int, give: Resources, get: Resources) extends PerfectInformationSOCMove[PortTradeMove]

object PortTradeMove {

  def generator[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: PortSOCStateImplicit[BOARD, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, Resource, PERSPECTIVE], PerfectInfo, PortTradeMove] =
    (state, perfectInfo, pos) => {
      import PortSOCState._
      def canSpend(res: Resource, amount: Int): Boolean = perfectInfo.canSpend(CatanSet.fromMap[Resource, Int](Map(res -> amount)))
      def generate(res: Resource, otherRes: Seq[Resource], amount: Int): Seq[PortTradeMove] = {
        val give = CatanSet.fromMap(Map(res -> amount))
        otherRes.map { r =>
          val get = CatanSet.fromMap(Map(r -> 1))
          PortTradeMove(pos, give, get)
        }
      }
      val ports = state.getPortsForPlayer(pos)
      val _3to1 = ports.contains(Misc)
      perfectInfo.itemSet.getTypes.flatMap { res =>
        val otherRes: Seq[Resource] = perfectInfo.itemSet.getTypes.filterNot(_ == res)
        if (ports.contains(res) && canSpend(res, 2)) generate(res, otherRes, 2)
        else if (_3to1 && canSpend(res, 3)) generate(res, otherRes, 3)
        else if (canSpend(res, 4)) generate(res, otherRes, 4)
        else Nil
      }.filter(state.canPortTrade)
    }

  def canDoMove[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: PortSOCStateImplicit[BOARD, PERSPECTIVE, STATE]): CanDoMove[STATE[BOARD, Resource, PERSPECTIVE], PerfectInfo, PortTradeMove] =
    (state, perfectInfo, move) => {
      import PortSOCState._
      state.canPortTrade(move) && perfectInfo.canSpend(move.give)
    }
}


//case class PortTradeAction[BOARD <: BoardConfiguration, STATE[P] <: PortSOCState[BOARD, P, STATE]]() extends PerfectInformationMoveGameAction[BOARD, Resource, STATE, PortTradeAction[BOARD, STATE]] {
//  override type A = PortTradeMove
//  override def canDoAction[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Boolean = {
//    inv.numCards >= 2 && state.getPortsForPlayer(position).length > 0
//  }
//  override def getAllMoves[PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo]](state: STATE[PERSPECTIVE], inv: PerfectInfo, position: Int): Seq[PortTradeMove] = {
//    def canSpend(res: Resource, amount: Int): Boolean = inv.canSpend(CatanSet.fromMap[Resource, Int](Map(res -> amount)))
//    def generate(res: Resource, otherRes: Seq[Resource], amount: Int) = {
//      val give = CatanSet.fromMap(Map(res -> amount))
//      otherRes.map { r =>
//        val get = CatanSet.fromMap(Map(r -> 1))
//        PortTradeMove(position, give, get)
//      }
//    }
//    val ports = state.getPortsForPlayer(position)
//    val _3to1 = ports.contains(Misc)
//    inv.itemSet.getTypes.flatMap { res =>
//      val otherRes: Seq[Resource] = inv.itemSet.getTypes.filterNot(_ == res)
//      if (ports.contains(res) && canSpend(res, 2)) generate(res, otherRes, 2)
//      else if (_3to1 && canSpend(res, 3)) generate(res, otherRes, 3)
//      else if (canSpend(res, 4)) generate(res, otherRes, 4)
//      else Nil
//    }.filter(state.canPortTrade)
//  }
//}

object PortSOCState {

//  type PortSOCStateImplicit[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE[B, I, P] <: HList] = (SOCBaseOps0[BOARD, Resource, PERSPECTIVE, STATE], BoardOps[STATE[BOARD, Resource, PERSPECTIVE]])
//  implicit def getImplicits[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE[B, I, P] <: HList](implicit baseOps: SOCBaseOps0[BOARD, Resource, PERSPECTIVE, STATE], boardOps: BoardOps[STATE[BOARD, Resource, PERSPECTIVE]]): PortSOCStateImplicit[BOARD, PERSPECTIVE, STATE] =
//    (baseOps, boardOps)

  implicit class PortSOCStateOps[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], STATE[B, I, P] <: HList](state: STATE[BOARD, Resource, PERSPECTIVE])(implicit dep: DependsOn[STATE[BOARD, Resource, PERSPECTIVE], SOCState[BOARD, Resource, PERSPECTIVE]], boardOps: BoardOps[STATE[BOARD, Resource, PERSPECTIVE]]) {

    import SOCState.SOCStateOps

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

    def doPortTrade(portTradeMove: PortTradeMove): S = state.updateTransactions(List(
      Lose(portTradeMove.player, portTradeMove.give),
      Gain(portTradeMove.player, portTradeMove.get),
      Gain(SOCState.BANK_PLAYER_ID, portTradeMove.give),
      Lose(SOCState.BANK_PLAYER_ID, portTradeMove.get),
    ))
  }
}

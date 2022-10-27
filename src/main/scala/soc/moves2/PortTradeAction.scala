package soc.moves2

import shapeless.HList
import soc.board.{BoardConfiguration, Edge, Vertex}
import soc.inventory.InventoryHelper.{PerfectInvHelper, perfectInfoInvFactory}
import soc.inventory.resources.{Gain, Lose, ResourceSet}
import soc.inventory.{CatanSet, Inventory, InventoryHelper, InventoryItem, Misc, PerfectInfoInventory, Port, Resource}
import soc.inventory.resources.ResourceSet.Resources
import soc.state.SOCState.SOCState
import soc.state.SOCState
import soc.state.build.{BoardOps, CitySOCState, SettlementSOCState}
import util.DependsOn

case class PortTradeMove(player: Int, give: Resources, get: Resources) extends PerfectInformationSOCMove[PortTradeMove]

//object PortTradeMove {
//
//  def generator[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: PortSOCStateImplicit[BOARD, PERSPECTIVE, STATE]): MoveGenerator[STATE[BOARD, Resource, PERSPECTIVE], PerfectInfo, PortTradeMove] =
//    (state, perfectInfo, pos) => {
//      import PortSOCState._
//      def canSpend(res: Resource, amount: Int): Boolean = perfectInfo.canSpend(CatanSet.fromMap[Resource, Int](Map(res -> amount)))
//      def generate(res: Resource, otherRes: Seq[Resource], amount: Int): Seq[PortTradeMove] = {
//        val give = CatanSet.fromMap(Map(res -> amount))
//        otherRes.map { r =>
//          val get = CatanSet.fromMap(Map(r -> 1))
//          PortTradeMove(pos, give, get)
//        }
//      }
//      val ports = state.getPortsForPlayer(pos)
//      val _3to1 = ports.contains(Misc)
//      perfectInfo.itemSet.getTypes.flatMap { res =>
//        val otherRes: Seq[Resource] = perfectInfo.itemSet.getTypes.filterNot(_ == res)
//        if (ports.contains(res) && canSpend(res, 2)) generate(res, otherRes, 2)
//        else if (_3to1 && canSpend(res, 3)) generate(res, otherRes, 3)
//        else if (canSpend(res, 4)) generate(res, otherRes, 4)
//        else Nil
//      }.filter(state.canPortTrade)
//    }
//
//  def canDoMove[BOARD <: BoardConfiguration, PERSPECTIVE <: InventoryHelper[Resource, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[Resource, PerfectInfo], STATE[B, I, P] <: HList](implicit ops: PortSOCStateImplicit[BOARD, PERSPECTIVE, STATE]): CanDoMove[STATE[BOARD, Resource, PERSPECTIVE], PerfectInfo, PortTradeMove] =
//    (state, perfectInfo, move) => {
//      import PortSOCState._
//      state.canPortTrade(move) && perfectInfo.canSpend(move.give)
//    }
//}


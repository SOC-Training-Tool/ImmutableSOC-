package soc.moves2.build

import shapeless.ops.hlist.{SelectAll, Selector}
import shapeless.{::, HList, HNil}
import soc.board.{BoardConfiguration, Vertex}
import soc.inventory.resources.{Gain, Lose}
import soc.inventory._
import soc.moves2.SOCState.{SOCState, _}
import soc.moves2._
import util.MapWrapper

case class BuildSettlementMove(player: Int, vertex: Vertex) extends PerfectInformationSOCMove

case class SOCSettlementMap(m: Map[Vertex, Settlement]) extends MapWrapper[Vertex, Settlement]

trait SettlementBoardOps[B, I, P, S] extends BoardOps[B, I, P, S] {
  def onBuildSettlement(buildSettlementMove: BuildSettlementMove, s: S)(f: S => S): S
}

object SettlementSOCState {

  implicit class SettlementSOCStateOps[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]) {

    implicit val socStateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]

    val settlements: SOCSettlementMap = dep.get(state)

    def updateSettlements(settlements: Map[Vertex, Settlement]): STATE = dep.update(SOCSettlementMap(settlements), state)

    def buildSettlement(buildSettlementMove: BuildSettlementMove, buy: Option[CatanSet[II, Int]]): STATE = settlementBoardOps.onBuildSettlement(buildSettlementMove, state) { state =>
      val pointsForPlayer = state.playerPoints(buildSettlementMove.player)
      val us = state.updateSettlements(state.settlements + (buildSettlementMove.vertex -> Settlement(buildSettlementMove.player)))
      buy.fold(us) { cost =>
        us.updateTransactions(List(Gain(SOCState.BANK_PLAYER_ID, cost), Lose(buildSettlementMove.player, cost)))
      }.updatePoints(SOCPlayerPointsMap((state.playerPoints - buildSettlementMove.player) + (buildSettlementMove.player -> (pointsForPlayer + 1))))
    }

    def settlementsForPlayer(player: Int): Int = settlements.values.count(_.playerId == player)

    def canPlaceFreeSettlement(loc: Vertex): Boolean = {
      state.board.vertices.contains(loc) &&
        !settlementBoardOps.vertexBuildingMap(state).contains(loc) &&
        state.board.neighboringVertices(loc).forall { v => !settlementBoardOps.vertexBuildingMap(state).contains(v) }
    }

    def canBuildSettlement(buildSettlementMove: BuildSettlementMove): Boolean = {
      canPlaceFreeSettlement(buildSettlementMove.vertex) && {
        state.board.edgesFromVertex(buildSettlementMove.vertex).exists { edge =>
          settlementBoardOps.edgeBuildingMap(state).get(edge).fold(false)(_.playerId == buildSettlementMove.player)
        }
      }
    }
  }

  implicit def moveGenerator[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE]): MoveGenerator[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
    (state: STATE, _: PerfectInfo, pos: Int) =>
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      state.board.vertices.map(BuildSettlementMove(pos, _)).filter(state.canBuildSettlement)
  }

  implicit def baseCanDoAction[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildSettlementMove]): CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
    (state, inv, player) =>
      import soc.moves2.RollDiceSOCState.RollDiceSOCStateOps
      implicit val stateDep = dep.innerDependency[SOCState[BOARD, II, PERSPECTIVE]]
      implicit val cityDep = dep.innerDependency[SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]]
      implicit val rollDep = dep.innerDependency[SOCCanRollDice :: SOCState[BOARD, II, PERSPECTIVE]]
      state.rolledDice && state.currentPlayer == player && state.settlementsForPlayer(player) < 4 && inv.canSpend(cost.getCost)
  }

  implicit def baseCanDoMove[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], PerfectInfo <: PerfectInfoInventory[II, PerfectInfo], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], canDoAction: CanDoAction[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove]): CanDoMove[BOARD, II, PERSPECTIVE, STATE, PerfectInfo, BuildSettlementMove] = {
    (state, inv, move) =>
      canDoAction(state, inv, move.player) && state.canBuildSettlement(move)
  }

  implicit def updateState[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], STATE <: HList](implicit dep: DependsOn[STATE, SOCSettlementMap :: SOCState[BOARD, II, PERSPECTIVE]], settlementBoardOps: SettlementBoardOps[BOARD, II, PERSPECTIVE, STATE], cost: Cost[II, BuildSettlementMove]): UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] = new UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE] {
    override def apply(t: STATE, u: BuildSettlementMove): STATE = t.buildSettlement(u, Some(cost.getCost))
  }

  type BASE_NEXT_MOVES[W[_ <: SOCMoveResult]] = W[BuildSettlementMove] :: W[BuildRoadMove] :: W[BuildCityMove] :: W[EndTurnMove] :: HNil // TODO add full list

  implicit def applyMoveResult[BOARD <: BoardConfiguration, II <: InventoryItem, PERSPECTIVE <: InventoryHelper[II, PERSPECTIVE], W[_ <: SOCMoveResult], A <: HList, STATE <: HList](implicit ws: Selector[A, W[BuildSettlementMove]], sa: SelectAll[A, BASE_NEXT_MOVES[W]], us: UpdateState[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE]): ApplyMoveResult[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE, W, A] =
    ApplyMoveResult.simpleApplyMoveResult[BOARD, II, PERSPECTIVE, BuildSettlementMove, STATE, W, A, BASE_NEXT_MOVES[W]]
}

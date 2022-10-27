package soc.state.build

import org.scalatest.FunSpec
import shapeless.{::, HNil}
import soc.CatanFixtures
import soc.board.BaseCatanBoard.BASE
import soc.board.{Edge, Vertex}
import soc.inventory.InventoryHelper.PerfectInfoInv
import soc.inventory.resources.{Gain, ResourceSet}
import soc.inventory.{City, EdgeBuilding, InventoryHelper, Resource, Road, Settlement, VertexBuilding}
import soc.moves2.build.BuildSettlementMove
import soc.state.SOCState.{SOCState, _}
import soc.state.{SOCPlayerPointsMap, SOCTurn}
import soc.state.build.SettlementSOCState._
import util.DependsOn

class SettlementSOCStateSpec extends FunSpec {

  type SettlementState = SOCSettlementMap :: SOCState[BASE, Resource, PerfectInfoInv[Resource]]

  val invHelper = InventoryHelper.perfectInfoInvFactory[Resource].create(List(0, 1, 2, 3))
  val board = CatanFixtures.baseBoard
  val bank = ResourceSet.apply(19, 19, 19, 19, 19)
  val turn = SOCTurn(0)
  val points = SOCPlayerPointsMap(Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0))

  val socState: SOCState[BASE, Resource, PerfectInfoInv[Resource]] = invHelper :: board :: bank :: turn :: points :: HNil
  val settlementMap = SOCSettlementMap(Map.empty)
  val state = settlementMap :: socState

  implicit val settlementDep = DependsOn[SOCSettlementMap :: SOCState[BASE, Resource, PerfectInfoInv[Resource]], SettlementState]
  implicit val dep = settlementDep.innerDependency[SOCState[BASE, Resource, PerfectInfoInv[Resource]]]

  val playerId = 0

  describe("canBuildSettlement") {

    describe("canPlaceFreeSettlement") {

      it("returns false when vertex is invalid") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps()

        assert(!state.canPlaceFreeSettlement(Vertex(-1)))
      }

      it("returns false when building exists on vertex") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps(v = Map(0 -> City(0)))

        assert(!state.canPlaceFreeSettlement(Vertex(0)))
      }

      it("returns false when a neighboring vertices is occupied") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps(v = Map(1 -> City(0)))

        assert(!state.canPlaceFreeSettlement(Vertex(0)))
      }

      it("returns true when settlement can be placed") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps()

        assert(state.canPlaceFreeSettlement(Vertex(0)))
      }
    }

    describe("cannot build settlement on vertex when") {
      it("there is no adjacent edge building") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps()

        assert(!state.canBuildSettlement(BuildSettlementMove(playerId, Vertex(0))))
      }

      it("no adjacent edge building has a matching playerId") {
        implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
          stubbedSettlementBoardOps(e = Map(
            Edge(Vertex(30), Vertex(47)) -> Road(1),
            Edge(Vertex(29), Vertex(30)) -> Road(2),
            Edge(Vertex(30), Vertex(31)) -> Road(3)))

        assert(!state.canBuildSettlement(BuildSettlementMove(playerId, Vertex(30))))
      }
    }

    it("can build settlement on vertex when there is an adjacent edge building with a matching playerId") {
      implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
        stubbedSettlementBoardOps(e = Map(
          Edge(Vertex(0), Vertex(1)) -> Road(playerId)))

      assert(!state.canBuildSettlement(BuildSettlementMove(playerId, Vertex(0))))
    }
  }

  describe("buildSettlement") {

    it("increments player's points") {
      implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
        stubbedSettlementBoardOps()

      val result = state.buildSettlement(BuildSettlementMove(playerId, Vertex(0)), None)
      assert(result.playerPoints.apply(playerId) == 1)
    }

    it("adds settlement to SOCSettlementMap state") {
      implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
        stubbedSettlementBoardOps()

      val result = state.buildSettlement(BuildSettlementMove(playerId, Vertex(0)), None)
      assert(result.settlementsForPlayer(playerId) == 1)
    }

    it("updates players inventory to subtract building cost") {
      implicit val settlementBoardOps: SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] =
        stubbedSettlementBoardOps()

      val result = state
        .updateTransactions(List(Gain(playerId, Settlement.cost)))
        .buildSettlement(BuildSettlementMove(playerId, Vertex(0)), Some(Settlement.cost))

      assert(result.playerInventories(playerId).itemSet.isEmpty)
    }
  }

  private def stubbedSettlementBoardOps(v: Map[Vertex, VertexBuilding] = Map.empty, e: Map[Edge, EdgeBuilding] = Map.empty) =
    new SettlementBoardOps[BASE, Resource, PerfectInfoInv[Resource], SettlementState] {
      override def onBuildSettlement(buildSettlementMove: BuildSettlementMove, s: SettlementState)(f: SettlementState => SettlementState): SettlementState = f(s)

      override def vertexBuildingMap(s: SettlementState): Map[Vertex, VertexBuilding] = v

      override def edgeBuildingMap(s: SettlementState): Map[Edge, EdgeBuilding] = e
    }
}
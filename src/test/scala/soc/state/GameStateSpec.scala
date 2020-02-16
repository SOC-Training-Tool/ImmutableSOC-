package soc.state

import org.scalatest.{FunSpec, Matchers, OptionValues}
import soc.CatanFixtures
import soc.board.{BaseBoardConfiguration, Edge, Vertex}
import soc.core.GameRules
import soc.inventory.Inventory.{PerfectInfo, ProbableInfo}
import soc.inventory._
import soc.moves._
import soc.moves.PossibleMoves._
//import soc.state.vectorize.SOCVectorize

class GameStateSpec extends FunSpec with Matchers with OptionValues{

  import soc.inventory.InventoryHelper._
  import soc.board.BaseCatanBoard._
  val board = CatanFixtures.baseBoard
  val players = Seq(("Player0", 0), ("Player1", 1), ("Player2", 2), ("Player3", 3))
  val rules = GameRules.default
  val gameState = GameState[PerfectInfo, BaseBoardConfiguration](board, players.map(_._2), rules)

  describe("initial placements") {

    it("should update board with initial placements vertices and edges") {
      val vertex = Vertex(0)
      val edge = Edge(Vertex(0), Vertex(1))

      val response = gameState.initialPlacement(InitialPlacementMove(true, vertex, edge)).state
      response.getPlayerInfo(_.points).get(0).value shouldBe 1
      response.board.verticesBuildingMap should contain only((vertex,Settlement(0)))
      response.board.edgesBuildingMap should contain only( (edge, Road(0)) )
      response.currentPlayer shouldBe gameState.players.nextPlayer(gameState.currentPlayer)
    }
  }

  describe ("full game") {

    it("perfectInfo") {
      val finishedState: GameState[PerfectInfo, BaseBoardConfiguration] = CatanFixtures.testMoveResults.foldLeft(gameState){ case (state, result) =>
        state.doMoveIfCan(result).state
      }
      assert(finishedState.isOver)
    }

    describe("probableInfo") {
      it("no perspective") {
        val probableGameState = GameState[ProbableInfo, BaseBoardConfiguration](board, players.map(_._2), rules)
        val finishedState: GameState[ProbableInfo, BaseBoardConfiguration] = CatanFixtures.noInfoPerspective.foldLeft(probableGameState){
          case (state, result) => state.doMoveIfCan(result).state
        }
        assert(finishedState.isOver)
      }

      it("from a single players perspective") {
        val probableGameState = GameState[ProbableInfo, BaseBoardConfiguration](board, players.map(_._2), rules)
        val player0Perspective = CatanFixtures.perspectiveMoveResults(Seq(0))(0)
        val finishedState: GameState[ProbableInfo, BaseBoardConfiguration] = player0Perspective.foldLeft(probableGameState){
          case (state, result) => state.doMoveIfCan(result).state
        }
        assert(finishedState.isOver)
      }
    }
  }
}

package soc.moves

import org.scalatest.{FunSpec, Matchers}
import soc.CatanFixtures
import soc.core.GameRules
import soc.inventory.Inventory.PerfectInfo
import soc.state.GameState

class CatanPossibleMovesSpec extends FunSpec with Matchers {

  import soc.inventory.InventoryHelper._
  val board = CatanFixtures.baseBoard
  val players = Seq(("Player0", 0), ("Player1", 1), ("Player2", 2), ("Player3", 3))
  implicit  val rules = GameRules.default
  val gameState = GameState[PerfectInfo](board, players.map(_._2), rules)

  val moveResults = CatanFixtures.testMoveResults

  describe("getPossibleDevelopmentCard") {

    it ("should not be able to play a development card the turn it bought it") {
      val index = moveResults.find(_.isInstanceOf[BuyDevelopmentCardResult]).map(a => moveResults.indexOf(a)).get
      val movesUpToBuyDevelopmentCard = moveResults.dropRight(moveResults.length - index - 1)
      val state = movesUpToBuyDevelopmentCard.foldLeft(gameState){ case (s, m) => s.apply(m).state }
      val playerPosition = state.currentPlayer
      val publicState = state.toPublicGameState
      val privateInventory = state.players.getPlayer(playerPosition).inventory

      val possibleDevCardMoves = CatanPossibleMoves(publicState, privateInventory, playerPosition).getPossibleDevelopmentCard
      possibleDevCardMoves shouldBe empty







    }

  }
}
